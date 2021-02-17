use syntect::{
    highlighting::ThemeSet,
    parsing::{SyntaxDefinition, SyntaxSet, SyntaxSetBuilder},
};

use crate::errors::{Error, HurlResult};

pub fn build() -> HurlResult<(SyntaxSet, ThemeSet)> {
    macro_rules! load_syntax_def {
        ($builder:ident, $file:expr) => {
            let syntax_def = include_str!($file);
            let def = SyntaxDefinition::load_from_str(syntax_def, true, None)
                .map_err(|_| Error::SyntaxLoadError($file))?;
            $builder.add(def);
        };
    }

    let mut builder = SyntaxSetBuilder::new();
    load_syntax_def!(builder, "../assets/HTTP.sublime-syntax");
    load_syntax_def!(builder, "../sublime/JSON/JSON.sublime-syntax");
    load_syntax_def!(builder, "../sublime/JavaScript/JavaScript.sublime-syntax");
    load_syntax_def!(builder, "../sublime/HTML/HTML.sublime-syntax");
    load_syntax_def!(builder, "../sublime/CSS/CSS.sublime-syntax");
    load_syntax_def!(builder, "../sublime/Markdown/Markdown.sublime-syntax");
    load_syntax_def!(builder, "../sublime/JSON/JSON.sublime-syntax");
    load_syntax_def!(builder, "../sublime/YAML/YAML.sublime-syntax");
    Ok((builder.build(), ThemeSet::load_defaults()))
}
