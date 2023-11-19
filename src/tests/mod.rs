#[cfg(test)]
mod tests {
    use anyhow::{bail, Result};
    use std::fs::{read_dir, DirEntry};
    use std::process::Command;

    #[test]
    fn execute_tests() {
        let cases = read_dir("src/tests/cases").unwrap();
        let mut count = 0;

        let mut errors = vec![];
        let mut msgs = vec![];
        for case in cases {
            let case = case.unwrap();
            let name = case.path().display().to_string();
            if name.ends_with(".jlox") {
                count += 1;

                match run_test(case) {
                    Ok(_) => {
                        msgs.push(format!("Running {name}... \x1b[32mok\x1b[0m", name = name));
                    }
                    Err(e) => {
                        errors.push(e);
                        msgs.push(format!(
                            "Running {name}... \x1b[31mfailed\x1b[0m",
                            name = name
                        ));
                    }
                }
            }
        }

        println!("Running {} tests", count);
        msgs.sort();
        for msg in msgs {
            println!("{}", msg);
        }

        if !errors.is_empty() {
            for error in &errors {
                eprintln!("{}", error);
            }
            panic!(
                "{} tests failed, and {} tests passed",
                errors.len(),
                count - errors.len()
            );
        }

        println!("All tests passed");
    }

    fn run_test(file: DirEntry) -> Result<()> {
        let file_name = file.file_name().into_string().unwrap();

        let output = Command::new("cargo")
            .arg("run")
            .arg(file.path())
            .output()
            .expect("failed to execute process");

        let lines = String::from_utf8_lossy(&output.stdout);
        let lines = lines.split("\n").collect::<Vec<&str>>();
        let lines = lines
            .iter()
            .filter(|line| !line.is_empty())
            .collect::<Vec<&&str>>();

        let expected = std::fs::read_to_string(file.path().with_extension("out")).unwrap();
        let expected = expected.split("\n").collect::<Vec<&str>>();
        let expected = expected
            .iter()
            .map(|line| line.trim_end_matches("\r"))
            .collect::<Vec<&str>>();
        let expected = expected
            .iter()
            .filter(|line| !line.is_empty())
            .collect::<Vec<&&str>>();

        if lines.len() != expected.len() {
            bail!(
                "{}: Expected {} lines, got {}: \nExpected: {:?}\nGot: {:?}",
                file_name,
                expected.len(),
                lines.len(),
                expected,
                lines
            );
        }

        for i in 0..lines.len() {
            if lines[i] != expected[i] {
                bail!(
                    "{}: Expected \"{}\", got \"{}\" at line {}",
                    file_name,
                    expected[i],
                    lines[i],
                    i + 1
                );
            }
        }

        Ok(())
    }
}
