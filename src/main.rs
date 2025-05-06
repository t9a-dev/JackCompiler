use std::{
    fs::File,
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
};

use anyhow::{Context, Result, anyhow};
use compilation_engine::CompilationEngine;
use jack_tokenizer::JackTokenizer;
use symbol_table::SymbolTable;
use vm_writer::VMWriter;

const JACK_FILE_EXTENSION: &str = "jack";
const OUTPUT_FILE_EXTENSION: &str = "xml";

fn main() -> Result<()> {
    if let Err(e) = jack_compiler(&parse_arg(std::env::args().collect())?) {
        eprintln!("{}", e);
        std::process::exit(1);
    }
    Ok(())
}

fn parse_arg(args: Vec<String>) -> Result<String> {
    let current_dir = "./".to_string();
    match args.get(1) {
        Some(arg) if arg.is_empty() => Ok(current_dir),
        Some(arg) => Ok(arg.to_string()),
        _ => Ok(current_dir),
    }
}

fn parse_compile_target_path(path: &Path) -> Result<Vec<PathBuf>> {
    let mut jack_files: Vec<PathBuf> = Vec::new();
    if path.is_dir() {
        for entry in path.read_dir()? {
            if let Ok(entry) = entry {
                if entry.path().is_file() {
                    match entry.path().extension() {
                        Some(file_extension) if file_extension == JACK_FILE_EXTENSION => {
                            jack_files.push(entry.path().to_path_buf());
                        }
                        _ => (),
                    }
                }
            }
        }
    } else {
        if let Some(extension) = path.extension() {
            if extension == JACK_FILE_EXTENSION {
                jack_files.push(path.to_path_buf());
            } else {
                return Err(anyhow!("un supported file: {:?}", path));
            }
        }
    }
    Ok(jack_files)
}

fn jack_compiler(path_str: &str) -> Result<()> {
    let path = Path::new(path_str);
    let analyze_target_paths = parse_compile_target_path(path)?;
    analyze_target_paths
        .iter()
        .try_for_each(|jack_file| -> Result<()> {
            let xml_output_file_path = jack_file.parent().unwrap().join(format!(
                "{}.{}",
                jack_file.file_stem().unwrap().to_string_lossy().to_string(),
                OUTPUT_FILE_EXTENSION
            ));
            let vm_output_file_path = jack_file.parent().unwrap().join(format!(
                "{}.{}",
                jack_file.file_stem().unwrap().to_string_lossy().to_string(),
                "vm"
            ));
            let xml_output_file = Arc::new(Mutex::new(File::create(&xml_output_file_path)?));
            let vm_output_file = Arc::new(Mutex::new(File::create(&vm_output_file_path)?));
            let tokenizer = JackTokenizer::new(File::open(jack_file)?)
                .context(format!("jack_toknizer initialize failed: {:?}", jack_file))?;
            let class_symbol_table = SymbolTable::new();
            let subroutine_symbol_table = SymbolTable::new();
            let mut compilation_engine = CompilationEngine::new(
                tokenizer,
                xml_output_file,
                VMWriter::new(vm_output_file),
                class_symbol_table,
                subroutine_symbol_table,
            )?;
            compilation_engine.compile_class()?;
            Ok(())
        })?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use anyhow::Ok;
    use compilation_engine::CompilationEngine;
    use pretty_assertions::assert_eq;
    use std::{
        fs::{self, File},
        io::Cursor,
    };
    use vm_writer::VMWriter;

    use rand::distr::{Alphanumeric, SampleString};
    use walkdir::WalkDir;

    use super::*;

    const TEST_DIR: &str = "target/test/data";
    const TEST_JACK_DIR: &str = "test_files/11/Square";

    fn setup_tracing() {
        static INIT: std::sync::Once = std::sync::Once::new();
        INIT.call_once(|| {
            tracing_subscriber::fmt()
                .with_max_level(tracing::Level::DEBUG)
                .with_test_writer()
                .init();
        });
    }

    fn create_test_file(test_dir: Option<&str>, test_file_extension: &str) -> Result<String> {
        let test_dir = test_dir.or(Some("target/test/data")).unwrap();
        fs::create_dir_all(test_dir)?;
        let mut test_file_name = Alphanumeric.sample_string(&mut rand::rng(), 5);
        test_file_name = format!("{}.{}", test_file_name, test_file_extension);
        let file_path = Path::new(test_dir).join(&test_file_name);
        File::create(&file_path)?;
        Ok(file_path.to_string_lossy().to_string())
    }

    fn find_files_with_extension(dir: &Path, extension: &str) -> Result<Vec<String>> {
        let mut paths: Vec<String> = Vec::new();

        for entry in WalkDir::new(dir)
            .into_iter()
            .filter_map(Result::ok)
            .filter(|e| e.file_type().is_file())
        {
            if let Some(ext) = entry.path().extension() {
                if ext == extension {
                    paths.push(entry.path().display().to_string());
                }
            }
        }

        Ok(paths)
    }

    #[test]
    fn playground() -> Result<()> {
        let jack_files = find_files_with_extension(Path::new(TEST_JACK_DIR), JACK_FILE_EXTENSION)?;
        println!("jack files: {:?}", jack_files);
        Ok(())
    }

    #[test]
    fn test_parse_analyze_target_path_when_dirctory() -> Result<()> {
        let test_files = vec![
            create_test_file(Some(TEST_DIR), JACK_FILE_EXTENSION)?,
            create_test_file(Some(TEST_DIR), JACK_FILE_EXTENSION)?,
        ];
        let mut expect: Vec<PathBuf> = test_files
            .iter()
            .map(|f| Path::new(f).to_path_buf())
            .collect();
        let mut actual = parse_compile_target_path(Path::new(TEST_DIR))?;

        assert_eq!(expect.sort(), actual.sort());

        test_files
            .iter()
            .try_for_each(|test_file| fs::remove_file(test_file))?;
        Ok(())
    }

    #[test]
    fn run_jack_compiler() -> Result<()> {
        setup_tracing();
        let jack_file_paths =
            find_files_with_extension(Path::new(TEST_JACK_DIR), JACK_FILE_EXTENSION)?;

        jack_file_paths
            .iter()
            .try_for_each(|jack_file_path| jack_compiler(&jack_file_path))?;

        Err(anyhow!("debugging"))
    }

    #[test]
    fn run_compile() -> Result<()> {
        setup_tracing();
        let jack_file_paths =
            find_files_with_extension(Path::new(TEST_JACK_DIR), JACK_FILE_EXTENSION)?;

        jack_file_paths.iter().try_for_each(|jack_file_path| {
            let output = Arc::new(Mutex::new(Cursor::new(Vec::new())));
            let jack_code = File::open(jack_file_path)?;
            let tokenizer = JackTokenizer::new(jack_code)?;
            let class_symbol_table = SymbolTable::new();
            let subroutine_symbol_table = SymbolTable::new();
            let mut compilation_engine = CompilationEngine::new(
                tokenizer,
                output.clone(),
                VMWriter::new(output.clone()),
                class_symbol_table,
                subroutine_symbol_table,
            )?;
            compilation_engine
                .compile_class()
                .context(format!("compilation file: {:?}", jack_file_path))?;
            let output = output.lock().unwrap();
            let _actual = String::from_utf8_lossy(output.get_ref());

            assert_eq!(compilation_engine.expressions, vec![]);
            // assert_eq!(expect, actual);
            Ok(())
        })?;
        Ok(())
    }
}
