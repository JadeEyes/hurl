use log::{debug, trace};
use std::path::PathBuf;
use std::{convert::TryFrom, unreachable};
use structopt::StructOpt;

use crate::config;
use crate::errors::{Error, HurlResult};
use crate::session::make_safe_pathname;

#[derive(StructOpt, Debug)]
#[structopt(name = "hurl")]
pub struct App {
    /// Activiate quiet mode.
    ///
    /// This overrides any verbose settings.
    #[structopt(short, long)]
    pub quiet: bool,

    /// Verbose mode (-v, -vv, -vvv, etc).
    #[structopt(short, long, parse(from_occurrences))]
    pub verbose: u8,

    /// Form mode.
    #[structopt(short, long)]
    pub form: bool,

    /// Basic authentication.
    ///
    /// A string of the form `username:password`. If only `username` is given
    /// then you will be prompted for a password. If you wish to use no password
    /// then use the form `user:`.
    #[structopt(short, long)]
    pub auth: Option<String>,

    /// Bearer token authentication.
    ///
    /// A token which will be send as "Bearer <token>" in
    /// the authorization header.
    #[structopt(short, long)]
    pub token: Option<String>,

    /// Default transport.
    ///
    /// If a URL is given without a transport, i.e. example.com/foo
    /// http will be used as the transport by default. If this flag
    /// is set, then https will be used instead.
    #[structopt(short, long)]
    pub secure: bool,

    /// The HTTP method to use, one of HEAD, GET, POST, PUT, PATCH, DELETE.
    #[structopt(subcommand)]
    pub cmd: Option<Method>,

    /// The URL to issue a request to if a method subcommand is not specified.
    pub url: Option<String>,

    /// The parameters for the request if a method subcommand is not specified.
    ///
    /// There are seven types of parameters that can be added to a command-line.
    /// Each type of parameter is distinguished by the unique separator between
    /// the key and value.
    ///
    /// Header -- key:value
    ///
    ///     e.g. X-API-TOKEN:abc123
    /// File Upload -- key@filename
    ///
    ///     e.g. foo:=@bar.json
    ///
    /// results in
    ///     
    ///     { "foo": { "bar": "this is from bar.json" } }
    /// Query Parameter -- key==value
    ///
    ///     e.g. foo:=@bar.json
    ///
    /// results in
    ///     
    ///     { "foo": { "bar": "this is from bar.json" } }
    /// Data Field -- key=value
    ///
    ///     e.g. foo:=@bar.json
    ///
    /// results in
    ///     
    ///     { "foo": { "bar": "this is from bar.json" } }
    /// Data Field from Fiel -- key=@filename
    ///
    ///     e.g. foo:=@bar.json
    ///
    /// results in
    ///     
    ///     { "foo": { "bar": "this is from bar.json" } }
    /// Raw JSON data where the value should be parsed to JSON first -- key:=val
    ///
    ///     e.g. foo:=@bar.json
    ///
    /// results in
    ///     
    ///     { "foo": { "bar": "this is from bar.json" } }
    /// Raw JSON Data From File -- key:=@filename
    ///
    ///     e.g. foo:=@bar.json
    ///
    /// results in
    ///     
    ///     { "foo": { "bar": "this is from bar.json" } }
    #[structopt(parse(try_from_str = parse_param))]
    pub parameters: Vec<Parameter>,

    /// Configuration file.
    ///
    /// A TOML file which is stored by default at HOME/.config/hurl/config
    /// where HOME is platform dependent.
    ///
    /// The file supports the following optional keys with the given types:
    /// * `verbose: u8`
    /// * `form: bool`
    /// * `auth: string`
    /// * `token: string`
    /// * `secure: bool`
    ///
    /// Each option has the same meaning as the correspondng configuration
    /// option with the same name. The verbose setting is a number from 0
    /// meaning no logging to 5 meaning maximal log output.
    #[structopt(short, long, env = "HURL_CONFIG", parse(from_os_str))]
    pub config: Option<PathBuf>,

    /// Session name.
    #[structopt(long)]
    pub session: Option<String>,

    /// Session storage location.
    #[structopt(long, parse(from_os_str))]
    pub session_dir: Option<PathBuf>,

    /// If true then use the stored session to augument the request,
    /// but do not modify what is stored.
    #[structopt(long)]
    pub read_only: bool,
}

impl App {
    pub fn validate(&mut self) -> HurlResult<()> {
        if self.cmd.is_none() && self.url.is_none() {
            return Err(Error::MissingUrlAndCommand);
        }
        Ok(())
    }

    pub fn log_level(&self) -> Option<&'static str> {
        if self.quiet || self.verbose <= 0 {
            None
        } else {
            match self.verbose {
                1 => Some("error"),
                2 => Some("warn"),
                3 => Some("info"),
                4 => Some("debug"),
                _ => Some("trace"),
            }
        }
    }

    pub fn process_config_file(&mut self) {
        let config_path = config::config_file(self);
        let config_opt = config::read_config_file(config_path);
        if let Some(mut config) = config_opt {
            if self.verbose == 0 {
                if let Some(v) = config.verbose {
                    self.verbose = v;
                }
            }
            if !self.form {
                if let Some(f) = config.form {
                    self.form = f;
                }
            }
            if !self.secure {
                if let Some(s) = config.secure {
                    self.secure = s;
                }
            }
            if self.auth.is_none() {
                self.auth = config.auth.take();
            }
            if self.token.is_none() {
                self.token = config.token.take();
            }
        }
    }

    pub fn host(&self) -> String {
        if let Some(url) = &self.url {
            make_safe_pathname(url)
        } else if let Some(cmd) = &self.cmd {
            make_safe_pathname(&cmd.data().url)
        } else {
            unreachable!();
        }
    }
}

#[derive(StructOpt, Debug)]
#[structopt(rename_all = "screaming_snake_case")]
pub enum Method {
    HEAD(MethodData),
    GET(MethodData),
    PUT(MethodData),
    POST(MethodData),
    PATCH(MethodData),
    DELETE(MethodData),
}

impl Method {
    pub fn data(&self) -> &MethodData {
        use Method::*;
        match self {
            HEAD(x) => x,
            GET(x) => x,
            PUT(x) => x,
            POST(x) => x,
            PATCH(x) => x,
            DELETE(x) => x,
        }
    }
}

impl From<&Method> for reqwest::Method {
    fn from(m: &Method) -> Self {
        match m {
            Method::HEAD(_) => reqwest::Method::HEAD,
            Method::GET(_) => reqwest::Method::GET,
            Method::PUT(_) => reqwest::Method::PUT,
            Method::POST(_) => reqwest::Method::POST,
            Method::PATCH(_) => reqwest::Method::PATCH,
            Method::DELETE(_) => reqwest::Method::DELETE,
        }
    }
}

#[derive(StructOpt, Debug)]
pub struct MethodData {
    pub url: String,
    #[structopt(parse(try_from_str = parse_param))]
    pub parameters: Vec<Parameter>,
}

#[derive(Debug)]
pub enum Parameter {
    // :
    Header { key: String, value: String },
    // =
    Data { key: String, value: String },
    // :=
    RawJsonData { key: String, value: String },
    // ==
    Query { key: String, value: String },
    // @
    FormFile { key: String, filename: String },
    // =@
    DataFile { key: String, filename: String },
    // :=@
    RawJsonDataFile { key: String, filename: String },
}

impl Parameter {
    pub fn is_form_file(&self) -> bool {
        matches!(*self, Parameter::FormFile { .. })
    }

    pub fn is_data(&self) -> bool {
        !matches!(*self, Parameter::Header { .. } | Parameter::Query { .. })
    }
}

/// Our final task in this module is to write our `parse_param` function which
/// will take a string and maybe turn it into a `Parameter`. In order to make
/// this task easier, we define a `Token` type.
#[derive(Debug)]
enum Token<'a> {
    Text(&'a str),
    Escape(char),
}

/// We have these special separator characters like `:` and `==`, but we need
/// some notion of escaping to allow those to appear in keys and values. We
/// further write a helper function to take string and parse it into a vector of
/// these tokens.
fn gather_escapes<'a>(src: &'a str) -> Vec<Token<'a>> {
    let mut tokens = Vec::new();
    let mut start = 0;
    let mut end = 0;
    let mut chars = src.chars();
    loop {
        if let Some(c) = chars.next() {
            if c != '\\' {
                end += 1;
                continue;
            }
            let b = chars.next();
            if let Some(c) = b {
                match c {
                    '\\' | '=' | '@' | ':' => {
                        if start != end {
                            tokens.push(Token::Text(&src[start..end]));
                        }
                        tokens.push(Token::Escape(c));
                        end += 2;
                        start = end;
                    }
                    _ => end += 2,
                }
            } else {
                tokens.push(Token::Text(&src[start..end + 1]));
                return tokens;
            }
        } else {
            if start != end {
                tokens.push(Token::Text(&src[start..end]));
            }
            return tokens;
        }
    }
}

/// Finally, we can write our `parse_param` function which gets a string from
/// the command line and tries to trun it into a `Parameter` or an appropriate
/// error.
fn parse_param(src: &str) -> HurlResult<Parameter> {
    debug!("Parsing: {}", src);
    let separators = [":=@", "=@", "==", ":=", "@", "=", ":"];
    let tokens = gather_escapes(src);
    let mut found = Vec::new();
    let mut idx = 0;
    for (i, token) in tokens.iter().enumerate() {
        match token {
            Token::Text(s) => {
                for sep in separators.iter() {
                    if let Some(n) = s.find(sep) {
                        found.push((n, sep));
                    }
                }
                if !found.is_empty() {
                    idx = i;
                    break;
                }
            }
            Token::Escape(_) => {}
        }
    }
    if found.is_empty() {
        return Err(Error::ParameterMissingSeparator(src.to_owned()));
    }
    found.sort_by(|(ai, asep), (bi, bsep)| ai.cmp(bi).then(bsep.len().cmp(&asep.len())));
    let sep = found.first().unwrap().1;
    trace!("Found separator: {}", sep);
    let mut key = String::new();
    let value = String::new();
    for (i, token) in tokens.iter().enumerate() {
        if i < idx {
            match token {
                Token::Text(s) => key.push_str(&s),
                Token::Escape(c) => {
                    key.push('\\');
                    key.push(*c);
                }
            }
        }
    }

    if let Ok(separator) = Separator::try_from(*sep) {
        match separator {
            Separator::At => Ok(Parameter::FormFile {
                key,
                filename: value,
            }),
            Separator::Equal => Ok(Parameter::Data { key, value }),
            Separator::Colon => Ok(Parameter::Header { key, value }),
            Separator::ColonEqual => Ok(Parameter::RawJsonData { key, value }),
            Separator::EqualEqual => Ok(Parameter::Query { key, value }),
            Separator::EqualAt => Ok(Parameter::DataFile {
                key,
                filename: value,
            }),
            Separator::Snail => Ok(Parameter::RawJsonDataFile {
                key,
                filename: value,
            }),
        }
    } else {
        unreachable!();
    }
}

#[derive(Debug)]
enum Separator {
    Colon,
    Equal,
    At,
    ColonEqual,
    EqualEqual,
    EqualAt,
    Snail,
}

impl TryFrom<&str> for Separator {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            ":" => Ok(Separator::Colon),
            "=" => Ok(Separator::Equal),
            "@" => Ok(Separator::At),
            ":=" => Ok(Separator::ColonEqual),
            "==" => Ok(Separator::EqualEqual),
            "=@" => Ok(Separator::EqualAt),
            ":=@" => Ok(Separator::Snail),
            _ => Err(()),
        }
    }
}
