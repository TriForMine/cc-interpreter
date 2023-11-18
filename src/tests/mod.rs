
#[cfg(test)]
mod tests {
    use std::fs::{DirEntry, read_dir};
    use std::process::Command;
    use anyhow::{bail, Result};

    #[test]
    fn execute_tests() {
        let cases = read_dir("src/tests/cases").unwrap();

        let mut errors = vec![];
        for case in cases {
            let case = case.unwrap();
            if case.path().extension().unwrap() == "jlox" {
                match run_test(case) {
                    Ok(_) => {},
                    Err(e) => errors.push(e)
                }
            }
        }

        if !errors.is_empty() {
            for error in &errors {
                eprintln!("{}", error);
            }
            panic!("{} tests failed", errors.len());
        }
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
        let lines = lines.iter().filter(|line| !line.is_empty()).collect::<Vec<&&str>>();

        let expected = std::fs::read_to_string(file.path().with_extension("out")).unwrap();
        let expected = expected.split("\n").collect::<Vec<&str>>();
        let expected = expected.iter().map(|line| line.trim_end_matches("\r")).collect::<Vec<&str>>();
        let expected = expected.iter().filter(|line| !line.is_empty()).collect::<Vec<&&str>>();


        if lines.len() != expected.len() {
            bail!("{}: Expected {} lines, got {}: \nExpected: {:?}\nGot: {:?}", file_name, expected.len(), lines.len(), expected, lines);
        }

        for i in 0..lines.len() {
            if lines[i] != expected[i] {
                bail!("{}: Expected \"{}\", got \"{}\" at line {}", file_name, expected[i], lines[i], i + 1);
            }
        }

        Ok(())
    }
}
