use clap::Parser;
use colored::Colorize;
use prettytable::{Table, row};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Parser)]
pub struct LinesOfCodeReporterOptions {
    #[arg(short, long, value_name = "SUMMARY", default_value = "false")]
    pub summary: bool,
    #[arg(short, long, value_name = "DETAILED", default_value = "false")]
    pub detailed: bool,
    #[arg(short, long, value_name = "PR_SUMMARY", default_value = "false")]
    pub compare_detailed: bool,
}

#[derive(Default, Serialize, Deserialize, Clone)]
pub struct LinesOfCodeReport {
    pub concrete: usize,
    pub std: usize,
    pub examples: usize,
    pub tests: usize,
    pub bench: usize,
}

pub fn pr_message(
    old_report: HashMap<String, usize>,
    new_report: HashMap<String, usize>,
) -> String {
    let sorted_file_paths = {
        let mut keys: Vec<_> = new_report.keys().collect();
        keys.sort();
        keys
    };

    let mut table = Table::new();

    table.add_row(row!["File", "Lines", "Diff"]);

    let mut total_lines_changed: i64 = 0;
    let mut total_lines_added: i64 = 0;
    let mut total_lines_removed: i64 = 0;

    for file_path in sorted_file_paths {
        let current_loc = *new_report.get(file_path).unwrap() as i64;
        let previous_loc = *old_report.get(file_path).unwrap_or(&0) as i64;
        let loc_diff = current_loc - previous_loc;

        if loc_diff == 0 {
            continue;
        }

        if loc_diff > 0 {
            total_lines_added += loc_diff;
        } else {
            total_lines_removed += loc_diff.abs();
        }

        total_lines_changed += loc_diff.abs();

        // remove "ethrex/" and everything before it
        const ETHREX_PREFIX: &str = "ethrex/";
        let file_path_printable = if let Some(idx) = file_path.find(ETHREX_PREFIX) {
            &file_path[idx + ETHREX_PREFIX.len()..]
        } else {
            file_path
        };

        table.add_row(row![
            file_path_printable,
            current_loc,
            match current_loc.cmp(&previous_loc) {
                std::cmp::Ordering::Greater => format!("+{loc_diff}"),
                std::cmp::Ordering::Less => format!("{loc_diff}"),
                std::cmp::Ordering::Equal => "-".to_owned(),
            }
        ]);
    }

    if total_lines_changed == 0 {
        return "".to_string();
    }

    let mut pr_message = String::new();

    pr_message.push_str("<h2>Lines of code report</h2>\n");
    pr_message.push('\n');

    pr_message.push_str(&pr_message_summary(
        total_lines_added,
        total_lines_removed,
        total_lines_changed,
    ));

    pr_message.push('\n');
    pr_message.push_str("<details>\n");
    pr_message.push_str("<summary>Detailed view</summary>\n");
    pr_message.push('\n');
    pr_message.push_str("```\n");
    pr_message.push_str(&format!("{table}\n"));
    pr_message.push_str("```\n");
    pr_message.push_str("</details>\n");

    pr_message
}

fn pr_message_summary(
    total_lines_added: i64,
    total_lines_removed: i64,
    total_lines_changed: i64,
) -> String {
    let mut pr_message = String::new();

    pr_message.push_str(&format!(
        "Total lines added: `{}`\n",
        match total_lines_added.cmp(&0) {
            std::cmp::Ordering::Greater => format!("{total_lines_added}"),
            std::cmp::Ordering::Less =>
                unreachable!("total_lines_added should never be less than 0"),
            std::cmp::Ordering::Equal => format!("{total_lines_added}"),
        }
    ));
    pr_message.push_str(&format!(
        "Total lines removed: `{}`\n",
        match total_lines_removed.cmp(&0) {
            std::cmp::Ordering::Greater | std::cmp::Ordering::Equal =>
                format!("{total_lines_removed}"),
            std::cmp::Ordering::Less =>
                unreachable!("total_lines_removed should never be less than 0"),
        }
    ));
    pr_message.push_str(&format!(
        "Total lines changed: `{}`\n",
        match total_lines_changed.cmp(&0) {
            std::cmp::Ordering::Greater | std::cmp::Ordering::Equal =>
                format!("{total_lines_changed}"),
            std::cmp::Ordering::Less =>
                unreachable!("total_lines_changed should never be less than 0"),
        }
    ));

    pr_message
}

pub fn slack_message(old_report: LinesOfCodeReport, new_report: LinesOfCodeReport) -> String {
    let concrete_diff = new_report.concrete.abs_diff(old_report.concrete);
    let std_diff = new_report.std.abs_diff(old_report.std);
    let examples_diff = new_report.examples.abs_diff(old_report.examples);
    let tests_diff = new_report.tests.abs_diff(old_report.tests);
    let bench_diff = new_report.bench.abs_diff(old_report.bench);
    let diff_total = concrete_diff + std_diff + examples_diff + tests_diff + bench_diff;

    format!(
        r#"{{
    "blocks": [
        {{
            "type": "header",
            "text": {{
                "type": "plain_text",
                "text": "Daily Lines of Code Report"
            }}
        }},
        {{
            "type": "divider"
        }},
        {{
            "type": "header",
            "text": {{
                "type": "plain_text",
                "text": "Summary"
            }}
        }},
        {{
            "type": "section",
            "text": {{
                "type": "mrkdwn",
                "text": "*concrete:* {} {}\n*std:* {} {}\n*examples:* {} {}\n*tests:* {} {}\n*bench:* {} {}\n*total diff:* {}"
            }}
        }}
    ]
}}"#,
        new_report.concrete,
        match new_report.concrete.cmp(&old_report.concrete) {
            std::cmp::Ordering::Greater => format!("(+{concrete_diff})"),
            std::cmp::Ordering::Less => format!("(-{concrete_diff})"),
            std::cmp::Ordering::Equal => "".to_string(),
        },
        new_report.std,
        match new_report.std.cmp(&old_report.std) {
            std::cmp::Ordering::Greater => format!("(+{std_diff})"),
            std::cmp::Ordering::Less => format!("(-{std_diff})"),
            std::cmp::Ordering::Equal => "".to_string(),
        },
        new_report.examples,
        match new_report.examples.cmp(&old_report.examples) {
            std::cmp::Ordering::Greater => format!("(+{examples_diff})"),
            std::cmp::Ordering::Less => format!("(-{examples_diff})"),
            std::cmp::Ordering::Equal => "".to_string(),
        },
        new_report.tests,
        match new_report.tests.cmp(&old_report.tests) {
            std::cmp::Ordering::Greater => format!("(+{tests_diff})"),
            std::cmp::Ordering::Less => format!("(-{tests_diff})"),
            std::cmp::Ordering::Equal => "".to_string(),
        },
        new_report.bench,
        match new_report.bench.cmp(&old_report.bench) {
            std::cmp::Ordering::Greater => format!("(+{bench_diff})"),
            std::cmp::Ordering::Less => format!("(-{bench_diff})"),
            std::cmp::Ordering::Equal => "".to_string(),
        },
        diff_total
    )
}

pub fn github_step_summary(old_report: LinesOfCodeReport, new_report: LinesOfCodeReport) -> String {
    let concrete_diff = new_report.concrete.abs_diff(old_report.concrete);
    let std_diff = new_report.std.abs_diff(old_report.std);
    let examples_diff = new_report.examples.abs_diff(old_report.examples);
    let tests_diff = new_report.tests.abs_diff(old_report.tests);
    let bench_diff = new_report.bench.abs_diff(old_report.bench);
    let diff_total = concrete_diff + std_diff + examples_diff + tests_diff + bench_diff;

    format!(
        r#"```
Concrete loc summary
====================
concrete: {} {}
std: {} {}
examples: {} {}
tests: {} {}
bench: {} {}
total diff: {}
```"#,
        new_report.concrete,
        match new_report.concrete.cmp(&old_report.concrete) {
            std::cmp::Ordering::Greater => format!("(+{concrete_diff})"),
            std::cmp::Ordering::Less => format!("(-{concrete_diff})"),
            std::cmp::Ordering::Equal => "".to_string(),
        },
        new_report.std,
        match new_report.std.cmp(&old_report.std) {
            std::cmp::Ordering::Greater => format!("(+{std_diff})"),
            std::cmp::Ordering::Less => format!("(-{std_diff})"),
            std::cmp::Ordering::Equal => "".to_string(),
        },
        new_report.examples,
        match new_report.examples.cmp(&old_report.examples) {
            std::cmp::Ordering::Greater => format!("(+{examples_diff})"),
            std::cmp::Ordering::Less => format!("(-{examples_diff})"),
            std::cmp::Ordering::Equal => "".to_string(),
        },
        new_report.tests,
        match new_report.tests.cmp(&old_report.tests) {
            std::cmp::Ordering::Greater => format!("(+{tests_diff})"),
            std::cmp::Ordering::Less => format!("(-{tests_diff})"),
            std::cmp::Ordering::Equal => "".to_string(),
        },
        new_report.bench,
        match new_report.bench.cmp(&old_report.bench) {
            std::cmp::Ordering::Greater => format!("(+{bench_diff})"),
            std::cmp::Ordering::Less => format!("(-{bench_diff})"),
            std::cmp::Ordering::Equal => "".to_string(),
        },
        diff_total
    )
}

pub fn shell_summary(new_report: LinesOfCodeReport) -> String {
    format!(
        "{}\n{}\n{} {}\n{} {}\n{} {}\n{} {}\n{} {}",
        "Lines of Code".bold(),
        "=============".bold(),
        "concrete:".bold(),
        new_report.concrete,
        "std:".bold(),
        new_report.std,
        "examples:".bold(),
        new_report.examples,
        "tests:".bold(),
        new_report.tests,
        "bench:".bold(),
        new_report.bench
    )
}
