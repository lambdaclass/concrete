use cfg::{
    Cfg, RuleContainer, Symbol,
    precedenced_rule::{Associativity, DEFAULT_ASSOC},
    rule::builder::RuleBuilder,
};
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    fmt,
    hash::{Hash, Hasher},
};
use syn::{
    Attribute, Fields, Ident, ItemEnum, ItemStruct, LitInt, LitStr, Token, Type, Variant,
    Visibility,
    parse::{Parse, ParseStream},
    parse_quote,
    punctuated::Punctuated,
};

#[derive(Debug)]
pub struct GrammarRules(Vec<GrammarRule>);

impl GrammarRules {
    pub fn iter(&self) -> impl Iterator<Item = &GrammarRule> {
        self.0.iter()
    }

    pub fn extract_types(&self) -> HashSet<Type> {
        let mut types = HashSet::new();
        for rule in &self.0 {
            types.insert(rule.self_ty());
            rule.extract_types(&mut types);
        }

        types
    }
}

impl Parse for GrammarRules {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut rules = Vec::new();
        while !input.is_empty() {
            rules.push(input.parse()?);
        }

        Ok(Self(rules))
    }
}

#[derive(Debug)]
pub enum GrammarRule {
    Alt(ItemEnum),
    Seq(ItemStruct),
}

impl GrammarRule {
    pub fn self_ty(&self) -> Type {
        let self_ty = match self {
            GrammarRule::Alt(item_enum) => &item_enum.ident,
            GrammarRule::Seq(item_struct) => &item_struct.ident,
        };

        parse_quote!(#self_ty)
    }

    pub fn attrs(&self) -> HashSet<String> {
        let attrs = match self {
            GrammarRule::Alt(item_enum) => &item_enum.attrs,
            GrammarRule::Seq(item_struct) => &item_struct.attrs,
        };

        attrs
            .iter()
            .filter(|&attr| {
                attr.path()
                    .get_ident()
                    .is_some_and(|ident| ident == "grammar")
            })
            .flat_map(|attr| {
                attr.parse_args_with(Punctuated::<Ident, Token![,]>::parse_terminated)
                    .unwrap()
                    .into_iter()
            })
            .map(|x| x.to_string())
            .collect::<HashSet<_>>()
    }

    fn extract_types(&self, types: &mut HashSet<Type>) {
        let mut process_fields = |fields: &Fields| match fields {
            Fields::Named(fields) => types.extend(fields.named.iter().map(|x| x.ty.clone())),
            Fields::Unnamed(fields) => types.extend(fields.unnamed.iter().map(|x| x.ty.clone())),
            Fields::Unit => todo!(),
        };

        match self {
            GrammarRule::Alt(item_enum) => {
                for variant in &item_enum.variants {
                    process_fields(&variant.fields);
                }
            }
            GrammarRule::Seq(item_struct) => {
                process_fields(&item_struct.fields);
            }
        }
    }

    pub fn build(&self, gram: &mut Cfg, mappings: &HashMap<Type, Symbol>) {
        let process_fields = |fields: &Fields| match fields {
            Fields::Named(fields) => fields
                .named
                .iter()
                .map(|x| mappings[&x.ty])
                .collect::<Vec<_>>(),
            Fields::Unnamed(fields) => fields
                .unnamed
                .iter()
                .map(|x| mappings[&x.ty])
                .collect::<Vec<_>>(),
            Fields::Unit => todo!(),
        };

        match self {
            GrammarRule::Alt(item_enum) => {
                if self.attrs().contains("precedenced") {
                    let mut variants = BTreeMap::<usize, HashSet<_>>::new();
                    for variant in &item_enum.variants {
                        let mut level = None;
                        let mut assoc = None;

                        for enum_attr in variant
                            .attrs
                            .iter()
                            .filter(|&attr| attr.path().get_ident().is_some_and(|x| x == "grammar"))
                            .flat_map(|attr| {
                                attr.parse_args_with(
                                    Punctuated::<EnumAttr, Token![,]>::parse_terminated,
                                )
                                .unwrap()
                                .into_iter()
                            })
                        {
                            match enum_attr {
                                EnumAttr::Assoc(x) => {
                                    assert!(assoc.is_none());
                                    assoc = Some(x);
                                }
                                EnumAttr::Level(x) => {
                                    assert!(level.is_none());
                                    level = Some(x);
                                }
                            }
                        }

                        variants
                            .entry(level.unwrap())
                            .or_default()
                            .insert(VariantWithAssoc { variant, assoc });
                    }

                    let mut builder = gram.precedenced_rule(mappings[&self.self_ty()]);
                    let mut current_level = *variants.first_entry().unwrap().key();
                    for (level, variants) in variants {
                        if current_level != level {
                            current_level = level;
                            builder = builder.lower_precedence();
                        }

                        for VariantWithAssoc { variant, assoc } in variants {
                            if let Some(assoc) = assoc {
                                builder = builder.associativity(assoc);
                            }

                            builder = builder.rhs(process_fields(&variant.fields));
                        }
                    }
                } else {
                    let mut builder = gram.rule(mappings[&self.self_ty()]);
                    for variant in &item_enum.variants {
                        builder = builder.rhs(process_fields(&variant.fields));
                    }
                }
            }
            GrammarRule::Seq(item_struct) => {
                gram.rule(mappings[&self.self_ty()])
                    .rhs(process_fields(&item_struct.fields));
            }
        }
    }
}

impl Parse for GrammarRule {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ahead = input.fork();

        let mut attrs = Vec::new();
        while ahead.peek(Token![#]) {
            attrs.push(Attribute::parse_outer(&ahead)?);
        }
        ahead.parse::<Visibility>()?;

        if ahead.peek(Token![struct]) {
            Ok(Self::Seq(input.parse()?))
        } else if ahead.peek(Token![enum]) {
            Ok(Self::Alt(input.parse()?))
        } else {
            todo!("{ahead:?}")
        }
    }
}

enum EnumAttr {
    Assoc(Associativity),
    Level(usize),
}

impl Parse for EnumAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let key: Ident = input.parse()?;
        input.parse::<Token![=]>()?;

        Ok(match key.to_string().as_str() {
            "level" => Self::Level(input.parse::<LitInt>()?.base10_parse()?),
            "assoc" => Self::Assoc(match input.parse::<LitStr>()?.value().as_str() {
                "left" => Associativity::Left,
                "right" => Associativity::Right,
                "group" => Associativity::Group,
                _ => todo!(),
            }),
            _ => todo!(),
        })
    }
}

struct VariantWithAssoc<'a> {
    variant: &'a Variant,
    assoc: Option<Associativity>,
}

impl Eq for VariantWithAssoc<'_> {}
impl PartialEq for VariantWithAssoc<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.variant.ident == other.variant.ident
    }
}

impl Hash for VariantWithAssoc<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.variant.ident.hash(state);
    }
}

impl fmt::Debug for VariantWithAssoc<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("VariantWithAssoc")
            .field("variant", &self.variant)
            .field(
                "assoc",
                &match self.assoc.unwrap_or(DEFAULT_ASSOC) {
                    Associativity::Left => "left",
                    Associativity::Right => "right",
                    Associativity::Group => "group",
                },
            )
            .finish()
    }
}
