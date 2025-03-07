use self::input::GrammarRules;
use cfg::{
    RuleContainer,
    predict::{FirstSets, FollowSets, PredictSets},
};
use proc_macro2::TokenStream;
use std::collections::HashMap;

mod codegen;
mod input;

pub fn impl_grammar(input: TokenStream) -> TokenStream {
    let rules: GrammarRules = syn::parse2(input).unwrap();

    //
    // Generate grammar specification.
    //
    let mut gram = cfg::Cfg::new();

    // Map each possible type to a symbol.
    let mappings = rules
        .extract_types()
        .into_iter()
        .map(|r#type| (r#type, gram.next_sym()))
        .collect::<HashMap<_, _>>();

    // Generate the grammar rules.
    for rule in rules.iter() {
        rule.build(&mut gram, &mappings);
    }

    let gram = gram.binarize();

    //
    // Generate first and follow sets.
    //
    let entry_rules = rules
        .iter()
        .filter(|&rule| rule.attrs().contains("entry"))
        .collect::<Vec<_>>();

    let first_sets = FirstSets::new(&gram);
    let first_sets = first_sets.predict_sets();

    assert_eq!(entry_rules.len(), 1, "multi-entry not supported (for now)");
    let follow_sets = FollowSets::new(&gram, mappings[&entry_rules[0].self_ty()], first_sets);
    let follow_sets = follow_sets.predict_sets();

    dbg!(&entry_rules);
    dbg!(first_sets);
    dbg!(follow_sets);

    // TODO: Generate parser (parser).
    // TODO: Generate parser (updater).

    // TODO: Generate visitor structs.

    TokenStream::new()
}
