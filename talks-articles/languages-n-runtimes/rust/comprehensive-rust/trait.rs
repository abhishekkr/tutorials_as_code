/*
`trait` => Abstract iver types; |||r to interfaces
*/

trait File {
    fn generate(&self);
}

// making File as supertrait that need to be impl when using File
trait SignedFile: File {
    fn sign(&self);
    fn fin(self)
    where
        Self: Sized,
    {
        println!("done.")
    }
}

#[derive(Debug)]
struct Txt {
    content: String,
}

impl File for Txt {
    fn generate(&self) {
        println!("[WIP] Saves {} to .txt file.", self.content);
    }
}
impl SignedFile for Txt {
    fn sign(&self) {
        println!("[WIP] Append Text of creator details.");
    }
}

// deriving trait Debug & Clone; popularly add Serialize via serde
#[derive(Debug, Clone)]
struct Docx {
    content: String,
}

impl File for Docx {
    fn generate(&self) {
        println!("[WIP] Saves {} to .docx file.", self.content);
    }
}
impl SignedFile for Docx {
    fn sign(&self) {
        println!("[WIP] Add docx details of creator.");
    }
}

fn dump_file(f: impl SignedFile) {
    f.generate();
    f.sign();
    f.fin();
}

fn main() {
    let txt = Txt {
        content: "Lorem".to_string(),
    };
    dump_file(txt);

    let docx = Docx {
        content: "Ipsum".to_string(),
    };
    let mut docy = docx.clone();
    dump_file(docx);

    docy.content = format!("Lorem {}", docy.content);
    dump_file(docy);
}
