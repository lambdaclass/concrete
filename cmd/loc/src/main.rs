use clap::Parser;
use report::{LinesOfCodeReport, LinesOfCodeReporterOptions, shell_summary};
use spinoff::{Color, Spinner, spinners::Dots};
use std::{
    collections::HashMap,
    path::PathBuf,
    process::{Command, Stdio},
};
use tokei::{Config, Language, LanguageType, Languages};

mod report;

fn count_loc(path: PathBuf, config: &Config) -> Option<Language> {
    let mut languages = Languages::new();
    languages.get_statistics(&[path], &[], config);
    languages.get(&LanguageType::Rust).cloned()
}

fn count_loc_other(path: PathBuf) -> usize {
    let mut first = Command::new("git")
        .arg("ls-files")
        .arg("-z")
        .arg(path)
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    first.wait().unwrap();
    let mut second = Command::new("xargs")
        .arg("-0")
        .arg("cat")
        .stdin(Stdio::from(first.stdout.unwrap()))
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    second.wait().unwrap();
    let third = Command::new("wc")
        .arg("-l")
        .stdin(Stdio::from(second.stdout.unwrap()))
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    let output = third.wait_with_output().unwrap();
    let result = std::str::from_utf8(&output.stdout).unwrap();
    result.trim().parse().unwrap()
}

fn main() {
    let opts = LinesOfCodeReporterOptions::parse();

    let mut spinner = Spinner::new(Dots, "Counting lines of code...", Color::Cyan);

    // Find the root of the ethrex repo
    let concrete_path = std::env::var("CARGO_MANIFEST_DIR")
        .map(PathBuf::from)
        .map(|path| path.parent().unwrap().parent().unwrap().to_path_buf())
        .unwrap();
    let concrete_src_path = concrete_path.join("src");
    let concrete_std_path = concrete_path.join("std");
    let concrete_examples_path = concrete_path.join("examples");
    let concrete_tests_path = concrete_path.join("tests");
    let concrete_bench_path = concrete_path.join("bench");

    let config = Config::default();

    let concrete_loc = count_loc(concrete_src_path, &config).unwrap();
    let std_loc = count_loc_other(concrete_std_path);
    let examples_loc = count_loc_other(concrete_examples_path);
    let tests_loc = count_loc(concrete_tests_path, &config).unwrap();
    let bench_loc = count_loc(concrete_bench_path, &config).unwrap();

    spinner.success("Lines of code calculated!");

    let mut spinner = Spinner::new(Dots, "Generating report...", Color::Cyan);

    let new_report = LinesOfCodeReport {
        concrete: concrete_loc.code,
        std: std_loc,
        examples: examples_loc,
        tests: tests_loc.code,
        bench: bench_loc.code,
    };

    if opts.detailed {
        let mut current_detailed_loc_report = HashMap::new();
        for report in concrete_loc.reports {
            let file_path = report.name;
            // let file_name = file_path.file_name().unwrap().to_str().unwrap();
            // let dir_path = file_path.parent().unwrap();

            current_detailed_loc_report
                .entry(file_path.as_os_str().to_str().unwrap().to_owned())
                .and_modify(|e: &mut usize| *e += report.stats.code)
                .or_insert_with(|| report.stats.code);
        }

        std::fs::write(
            "current_detailed_loc_report.json",
            serde_json::to_string(&current_detailed_loc_report).unwrap(),
        )
        .expect("current_detailed_loc_report.json could not be written");
    } else if opts.compare_detailed {
        let current_detailed_loc_report: HashMap<String, usize> =
            std::fs::read_to_string("current_detailed_loc_report.json")
                .map(|s| serde_json::from_str(&s).unwrap())
                .expect("current_detailed_loc_report.json could not be read");

        let previous_detailed_loc_report: HashMap<String, usize> =
            std::fs::read_to_string("previous_detailed_loc_report.json")
                .map(|s| serde_json::from_str(&s).unwrap())
                .unwrap_or(current_detailed_loc_report.clone());

        std::fs::write(
            "detailed_loc_report.txt",
            report::pr_message(previous_detailed_loc_report, current_detailed_loc_report),
        )
        .unwrap();
    } else if opts.summary {
        spinner.success("Report generated!");
        println!("{}", shell_summary(new_report));
    } else {
        std::fs::write(
            "loc_report.json",
            serde_json::to_string(&new_report).unwrap(),
        )
        .expect("loc_report.json could not be written");

        let old_report: LinesOfCodeReport = std::fs::read_to_string("loc_report.json.old")
            .map(|s| serde_json::from_str(&s).unwrap())
            .unwrap_or(new_report.clone());

        std::fs::write(
            "loc_report_slack.txt",
            report::slack_message(old_report.clone(), new_report.clone()),
        )
        .unwrap();
        std::fs::write(
            "loc_report_github.txt",
            report::github_step_summary(old_report, new_report),
        )
        .unwrap();

        spinner.success("Report generated!");
    }
}
