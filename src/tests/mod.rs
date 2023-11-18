#[cfg(test)]
mod tests {
    use std::process::Command;

    #[test]
    fn interpret_block() {
        let output = Command::new("cargo")
            .arg("run")
            .arg("src/tests/cases/block.jlox")
            .output()
            .expect("failed to execute process");

        let lines = String::from_utf8_lossy(&output.stdout);
        let lines = lines.split("\n").collect::<Vec<&str>>();

        assert_eq!(lines.len(), 3);
        assert_eq!(lines[0], "3");
        assert_eq!(lines[1], "3");
    }

    #[test]
    fn interpret_while() {
        let output = Command::new("cargo")
            .arg("run")
            .arg("src/tests/cases/while.jlox")
            .output()
            .expect("failed to execute process");

        let lines = String::from_utf8_lossy(&output.stdout);
        let lines = lines.split("\n").collect::<Vec<&str>>();

        assert_eq!(lines.len(), 3);
        assert_eq!(lines[0], "1");
        assert_eq!(lines[1], "0");
    }

    #[test]
    fn interpret_while_math() {
        let output = Command::new("cargo")
            .arg("run")
            .arg("src/tests/cases/while_math.jlox")
            .output()
            .expect("failed to execute process");

        let lines = String::from_utf8_lossy(&output.stdout);
        let lines = lines.split("\n").collect::<Vec<&str>>();

        assert_eq!(lines.len(), 11);
        assert_eq!(lines[0], "10");
        assert_eq!(lines[1], "90");
        assert_eq!(lines[2], "720");
        assert_eq!(lines[3], "5040");
        assert_eq!(lines[4], "30240");
        assert_eq!(lines[5], "151200");
        assert_eq!(lines[6], "604800");
        assert_eq!(lines[7], "1814400");
        assert_eq!(lines[8], "3628800");
        assert_eq!(lines[9], "3628800");
    }

    #[test]
    fn interpret_forloop() {
        let output = Command::new("cargo")
            .arg("run")
            .arg("src/tests/cases/forloop.jlox")
            .output()
            .expect("failed to execute process");

        let lines = String::from_utf8_lossy(&output.stdout);
        let lines = lines.split("\n").collect::<Vec<&str>>();

        let fibo = vec![
            "0", "1", "1", "2", "3", "5", "8", "13", "21", "34", "55", "89", "144", "233", "377",
            "610", "987", "1597", "2584", "4181", "6765", "10946",
        ];

        assert_eq!(lines.len(), fibo.len());
        for i in 0..20 {
            assert_eq!(lines[i], fibo[i]);
        }
    }

    #[test]
    fn interpret_fundef() {
        let output = Command::new("cargo")
            .arg("run")
            .arg("src/tests/cases/fundef.jlox")
            .output()
            .expect("failed to execute process");

        let lines = String::from_utf8_lossy(&output.stdout);
        let lines = lines.split("\n").collect::<Vec<&str>>();

        assert_eq!(lines.len(), 4);
        assert_eq!(lines[0], "1");
        assert_eq!(lines[1], "2");
        assert_eq!(lines[2], "3");
    }

    #[test]
    fn interpret_funreturn() {
        let output = Command::new("cargo")
            .arg("run")
            .arg("src/tests/cases/funreturn.jlox")
            .output()
            .expect("failed to execute process");

        let lines = String::from_utf8_lossy(&output.stdout);
        let lines = lines.split("\n").collect::<Vec<&str>>();

        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0], "5");
    }

    #[test]
    fn interpret_funreturnnil() {
        let output = Command::new("cargo")
            .arg("run")
            .arg("src/tests/cases/funreturnnil.jlox")
            .output()
            .expect("failed to execute process");

        let lines = String::from_utf8_lossy(&output.stdout);
        let lines = lines.split("\n").collect::<Vec<&str>>();

        assert_eq!(lines.len(), 4);
        assert_eq!(lines[0], "1");
        assert_eq!(lines[1], "2");
        assert_eq!(lines[2], "nil");
    }

    #[test]
    fn interpret_funverynested() {
        let output = Command::new("cargo")
            .arg("run")
            .arg("src/tests/cases/funverynested.jlox")
            .output()
            .expect("failed to execute process");

        let lines = String::from_utf8_lossy(&output.stdout);
        let lines = lines.split("\n").collect::<Vec<&str>>();

        assert_eq!(lines.len(), 3);
        assert_eq!(lines[0], "2");
        assert_eq!(lines[1], "3");
    }

    #[test]
    fn interpret_funclosure() {
        let output = Command::new("cargo")
            .arg("run")
            .arg("src/tests/cases/funclosure.jlox")
            .output()
            .expect("failed to execute process");

        let lines = String::from_utf8_lossy(&output.stdout);
        let lines = lines.split("\n").collect::<Vec<&str>>();

        assert_eq!(lines.len(), 5);
        assert_eq!(lines[0], "1");
        assert_eq!(lines[1], "2");
        assert_eq!(lines[2], "1");
        assert_eq!(lines[3], "2");
    }

    #[test]
    fn interpret_funanon() {
        let output = Command::new("cargo")
            .arg("run")
            .arg("src/tests/cases/funanon.jlox")
            .output()
            .expect("failed to execute process");

        let lines = String::from_utf8_lossy(&output.stdout);
        let lines = lines.split("\n").collect::<Vec<&str>>();

        println!("{:?}", lines);

        assert_eq!(lines.len(), 4);
        assert_eq!(lines[0], "1");
        assert_eq!(lines[1], "2");
        assert_eq!(lines[2], "3");
    }

    #[test]
    fn interpret_funanon2() {
        let output = Command::new("cargo")
            .arg("run")
            .arg("src/tests/cases/funanon2.jlox")
            .output()
            .expect("failed to execute process");

        let lines = String::from_utf8_lossy(&output.stdout);
        let lines = lines.split("\n").collect::<Vec<&str>>();

        println!("{:?}", lines);

        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0], "1");
    }
}
