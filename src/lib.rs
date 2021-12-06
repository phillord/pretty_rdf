use indexmap::IndexMap;
use quick_xml::{Writer, events::{BytesDecl, BytesEnd, BytesStart, BytesText, Event}};
use rio_api::model::{BlankNode, Literal, NamedNode, Subject, Term, Triple};
use std::{cmp::Ordering, collections::{HashMap, HashSet, VecDeque}, fmt::{Debug, Formatter}};
use std::{self, cell::RefCell, fmt,
          hash::{Hash,Hasher},
          io::{self, Write}};

// Utilities
pub fn is_name_start_char(c: char) -> bool {
    // ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
    matches!(c,
        ':'
        | 'A'..='Z'
        | '_'
        | 'a'..='z'
        | '\u{C0}'..='\u{D6}'
        | '\u{D8}'..='\u{F6}'
        | '\u{F8}'..='\u{2FF}'
        | '\u{370}'..='\u{37D}'
        | '\u{37F}'..='\u{1FFF}'
        | '\u{200C}'..='\u{200D}'
        | '\u{2070}'..='\u{218F}'
        | '\u{2C00}'..='\u{2FEF}'
        | '\u{3001}'..='\u{D7FF}'
        | '\u{F900}'..='\u{FDCF}'
        | '\u{FDF0}'..='\u{FFFD}'
        | '\u{10000}'..='\u{EFFFF}')
}

pub fn is_name_char(c: char) -> bool {
    // NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
    is_name_start_char(c)
        || matches!(c,  '-' | '.' | '0'..='9' | '\u{B7}' | '\u{0300}'..='\u{036F}' | '\u{203F}'..='\u{2040}')
}

fn map_err(error: quick_xml::Error) -> io::Error {
    if let quick_xml::Error::Io(error) = error {
        error
    } else {
        io::Error::new(io::ErrorKind::Other, error)
    }
}


// We need a complete copy of the whole data model because we need to
// be able to copy and cache items without worrying too much about
// lifetimes and without allocation. AsRef<str> supports both of
// this, assuming that there is an Rc in the way somewhere

#[derive(Ord, PartialOrd, Clone)]
pub struct PNamedNode<A:AsRef<str>> {
    pub iri: A,
    position_cache: RefCell<bool>,
    position_base: RefCell<Option<usize>>,
    position_add: RefCell<Option<usize>>,
}

impl<A:AsRef<str>> PNamedNode<A> {
    pub fn new(iri: A) -> Self {
        PNamedNode{iri, position_cache: RefCell::new(false),
                       position_base: RefCell::new(None),
                       position_add: RefCell::new(None)}
    }
}

impl <A:Debug + AsRef<str>> Debug for PNamedNode<A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> ::core::fmt::Result {
        match *self {
            PNamedNode {
                ref iri,
                position_cache: _,
                position_base: _,
                position_add:_
            } => {
                let mut debug_trait_builder =
                    f.debug_struct("PNamedNode");
                let _ = debug_trait_builder.field("iri", &&(*iri));
                debug_trait_builder.finish()
            }
        }
    }
}

impl<A:AsRef<str>> Hash for PNamedNode<A> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.iri.as_ref().hash(state);
    }
}

impl<A:AsRef<str>> PartialEq for PNamedNode<A> {
    fn eq(&self, other: &Self) -> bool {
        self.iri.as_ref() == other.iri.as_ref()
    }
}

impl<A:AsRef<str>> Eq for PNamedNode<A> {}


impl<A:AsRef<str>> PNamedNode<A> {
    fn split_iri(&self) -> (&str, &str) {
        let iri = self.iri.as_ref();

        let mut position_cache = self.position_cache.borrow_mut();
        let mut position_base = self.position_base.borrow_mut();
        let mut position_add = self.position_add.borrow_mut();

        if !*position_cache {
            *position_cache = true;
            *position_base = iri.rfind(|c| !is_name_char(c) || c == ':');
            if let Some(position_base) = *position_base {
                *position_add = iri[position_base..].find(|c| is_name_start_char(c) && c != ':')
            }
        }

        if let Some(position_base) = *position_base {
            if let Some(position_add) = *position_add {
                (
                    &iri[..position_base + position_add],
                    &iri[position_base + position_add..],
                )
            } else {
                (iri, "")
            }
        } else {
            (iri, "")
        }
    }
}

impl<A:AsRef<str>> fmt::Display for PNamedNode<A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let nn:NamedNode<'_> = self.into();
        write!(f, "{}", nn)
    }
}

impl<'a, A:AsRef<str>> From<&'a PNamedNode<A>> for NamedNode<'a> {
    fn from(arnn: &'a PNamedNode<A>) -> Self {
        NamedNode{iri: arnn.iri.as_ref()}
    }
}

impl From<NamedNode<'_>> for PNamedNode<String> {
    fn from(nn: NamedNode<'_>) -> Self {
        let iri: String = nn.iri.to_string();
        PNamedNode::new(iri)
    }
}

impl<A:AsRef<str>> AsRef<str> for PNamedNode<A> {
    fn as_ref(&self) -> &str {
        self.iri.as_ref()
    }
}


#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Hash)]
pub struct PBlankNode<A:AsRef<str>> {
    pub id: A,
}

impl<A:AsRef<str>> PBlankNode<A> {
    pub fn new(id: A) -> Self {
        PBlankNode{id}
    }
}

impl<A:AsRef<str>> fmt::Display for PBlankNode<A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let nn:BlankNode<'_> = self.into();
        write!(f, "{}", nn)
    }
}

impl<'a, A:AsRef<str>> From<&'a PBlankNode<A>> for BlankNode<'a> {
    fn from(arbn: &'a PBlankNode<A>) -> Self {
        BlankNode{id: arbn.id.as_ref()}
    }
}

impl From<BlankNode<'_>> for PBlankNode<String> {
    fn from(bn: BlankNode<'_>) -> Self {
        PBlankNode{id:bn.id.to_string()}
    }
}

impl<A:AsRef<str>> AsRef<str> for PBlankNode<A> {
    fn as_ref(&self) -> &str {
        self.id.as_ref()
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum PLiteral<A:AsRef<str>> {
    Simple {
        value: A,
    },
    LanguageTaggedString {
        value: A,
        language: A,
    },
    Typed {
        value: A,
        datatype: PNamedNode<A>,
    },
}

impl<A:AsRef<str>> fmt::Display for PLiteral<A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let nn:Literal<'_> = self.into();
        write!(f, "{}", nn)
    }
}

impl<'a, A:AsRef<str>> From<&'a PLiteral<A>> for Literal<'a> {
    fn from(l: &'a PLiteral<A>) -> Self {
        match l {
            PLiteral::Simple { value } =>
                Literal::Simple{value:value.as_ref()},
            PLiteral::LanguageTaggedString { value, language } =>
                Literal::LanguageTaggedString{
                    value: value.as_ref(),
                    language: language.as_ref(),
                },
            PLiteral::Typed { value, datatype } =>
                Literal::Typed {
                    value: value.as_ref(),
                    datatype: datatype.into(),
                },
        }
    }
}


impl From<Literal<'_>> for PLiteral<String> {
    fn from(l: Literal<'_>) -> Self {
        match l {
            Literal::Simple { value } =>
                PLiteral::Simple{value:value.to_string()},
            Literal::LanguageTaggedString { value, language } =>
                PLiteral::LanguageTaggedString{
                    value: value.to_string(),
                    language: language.to_string(),
                },
            Literal::Typed { value, datatype } =>
                PLiteral::Typed {
                    value: value.to_string(),
                    datatype: datatype.into(),
                },
        }
    }
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Hash)]
pub enum PSubject<A:AsRef<str>> {
    NamedNode(PNamedNode<A>),
    BlankNode(PBlankNode<A>),
}

impl<A:AsRef<str>> fmt::Display for PSubject<A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let nn:Subject<'_> = self.into();
        write!(f, "{}", nn)
    }
}

impl<'a, A:AsRef<str>> From<&'a PSubject<A>> for Subject<'a> {
    fn from(anbn: &'a PSubject<A>) -> Self {
        match anbn {
            PSubject::NamedNode(nn) =>
                Subject::NamedNode(nn.into()),
            PSubject::BlankNode(bn) =>
                Subject::BlankNode(bn.into())
        }
    }
}

impl From<Subject<'_>> for PSubject<String> {
    fn from(nbn: Subject<'_>) -> Self {
        match nbn {
            Subject::NamedNode(nn) =>
                PSubject::NamedNode(nn.into()),
            Subject::BlankNode(bn) =>
                PSubject::BlankNode(bn.into()),
            Subject::Triple(_) =>
                panic!("Subject triples are not supported")
        }
    }
}

impl<A:AsRef<str>> From<PNamedNode<A>> for PSubject<A> {
    fn from(nn: PNamedNode<A>) -> Self {
        PSubject::NamedNode(nn)
    }
}

impl<A:AsRef<str>> From<PBlankNode<A>> for PSubject<A> {
    fn from(nn: PBlankNode<A>) -> Self {
        PSubject::BlankNode(nn)
    }
}

impl<A:AsRef<str>> AsRef<str> for PSubject<A> {
    fn as_ref(&self) -> &str {
        match self {
            PSubject::NamedNode(nn) => nn.as_ref(),
            PSubject::BlankNode(bn) => bn.as_ref(),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum PTerm<A:AsRef<str>> {
    NamedNode(PNamedNode<A>),
    BlankNode(PBlankNode<A>),
    Literal(PLiteral<A>),
}

impl<A:AsRef<str>> PartialEq<PSubject<A>> for PTerm<A> {
    fn eq(&self, other: &PSubject<A>) -> bool {
        match (self, other) {
            (Self::NamedNode(nn), PSubject::NamedNode(onn))
                => nn.iri.as_ref() == onn.iri.as_ref(),
            (Self::BlankNode(bn), PSubject::BlankNode(obn))
                => bn.id.as_ref() == obn.id.as_ref(),
            _ => false
        }
    }
}

impl<A:AsRef<str>> From<PSubject<A>> for PTerm<A> {
    fn from(nbn: PSubject<A>) -> Self {
        match nbn {
            PSubject::NamedNode(nn) => nn.into(),
            PSubject::BlankNode(bn) => bn.into(),
        }
    }
}

impl<A:AsRef<str>> fmt::Display for PTerm<A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let t:Term<'_> = self.into();
        write!(f, "{}", t)
    }
}

impl<'a, A:AsRef<str>> From<&'a PTerm<A>> for Term<'a> {
    fn from(t: &'a PTerm<A>) -> Self {
        match t {
            PTerm::NamedNode(nn) =>
                Term::NamedNode(nn.into()),
            PTerm::BlankNode(bn) =>
                Term::BlankNode(bn.into()),
            PTerm::Literal(l) =>
                Term::Literal(l.into()),
        }
    }
}

impl From<Term<'_>> for PTerm<String> {
    fn from(t: Term<'_>) -> Self {
        match t {
            Term::NamedNode(nn) =>
                PTerm::NamedNode(nn.into()),
            Term::BlankNode(bn) =>
                PTerm::BlankNode(bn.into()),
            Term::Literal(l) =>
                PTerm::Literal(l.into()),
            Term::Triple(_) =>
                panic!("Subject Triples are not supported")
        }
    }
}

impl<A:AsRef<str>> From<PBlankNode<A>> for PTerm<A> {
    fn from(nn: PBlankNode<A>) -> Self {
        PTerm::BlankNode(nn)
    }
}

impl<A:AsRef<str>> From<PNamedNode<A>> for PTerm<A> {
    fn from(nn: PNamedNode<A>) -> Self {
        PTerm::NamedNode(nn)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct PTriple<A: AsRef<str>> {
    pub subject: PSubject<A>,
    pub predicate: PNamedNode<A>,
    pub object: PTerm<A>
}

impl<A:AsRef<str>> PTriple<A> {
    pub fn new(subject:PSubject<A>,
               predicate:PNamedNode<A>,
               object:PTerm<A>) -> PTriple<A> {
        PTriple{subject, predicate, object}
    }

    pub fn is_type(&self) -> bool {
        self.predicate.iri.as_ref() == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
    }

    pub fn is_collection(&self) -> bool {
        self.is_collection_first() || self.is_collection_rest()
    }

    pub fn is_collection_first(&self) -> bool {
        &self.predicate.iri.as_ref() == &"http://www.w3.org/1999/02/22-rdf-syntax-ns#first"
    }

    pub fn is_collection_rest(&self) -> bool {
        &self.predicate.iri.as_ref() == &"http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"
    }

    pub fn is_collection_end(&self) -> bool {
        if let PTerm::NamedNode(nn) = &self.object {
            nn.iri.as_ref() == "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"
        } else {
            false
        }
    }

    pub fn printable(&self) -> String {
        format!(
            "{}\n\t{}\n\t{}",
            self.subject,
            self.predicate,
            self.object
        )
    }
}

impl<A:AsRef<str>> fmt::Display for PTriple<A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let t:Triple<'_> = self.into();
        write!(f, "{}", t)
    }
}

impl<'a, A:AsRef<str>> From<&'a PTriple<A>> for Triple<'a>{
    fn from(t: &'a PTriple<A>) -> Self {
        Triple {
            subject: (&t.subject).into(),
            predicate: (&t.predicate).into(),
            object: (&t.object).into()
        }
    }
}

impl From<Triple<'_>> for PTriple<String> {
    fn from(t: Triple<'_>) -> Self {
        PTriple {
            subject: t.subject.into(),
            predicate: t.predicate.into(),
            object: t.object.into()
        }
    }
}

trait TripleLike<A>
    where A:AsRef<str> + Clone
{
    /// Can a new Triple be accepted onto this TripleLike.
    fn accept(&mut self, t:PTriple<A>) -> Option<PTriple<A>>;

    /// What is the subject of the triple like
    fn subject(&self) -> &PSubject<A>;

    fn literal_objects(&self) -> Vec<&PTriple<A>>;

    fn find_typed(&self) -> Option<&PTriple<A>>;
}

// A set of triples with a shared subject
// All the triples in `vec` should start with `subject`.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct PMultiTriple<A:AsRef<str>> {
    vec: Vec<PTriple<A>>,
}

impl<A> PMultiTriple<A>
where A: AsRef<str> + PartialEq
{
    #[allow(dead_code)]
    pub (crate) fn empty() -> PMultiTriple<A> {
        PMultiTriple{vec:vec![]}
    }

    pub fn new(vec:Vec<PTriple<A>>) -> PMultiTriple<A> {
        PMultiTriple{vec}
    }

    pub fn len(&self) -> usize {
        self.vec.len()
    }
}

impl<A> TripleLike<A> for PMultiTriple<A>
where A: AsRef<str> + Clone + PartialEq
{
    fn accept(&mut self, t:PTriple<A>) -> Option<PTriple<A>> {
        if self.subject().as_ref() == t.subject.as_ref() {
            self.vec.push(t);
            None
        } else {
            Some(t)
        }
    }

    fn subject(&self) -> &PSubject<A> {
        // There should be no empty instances, so this should be safe
        &self.vec[0].subject
    }

    fn literal_objects(&self) -> Vec<&PTriple<A>> {
        self.vec.iter().filter(|t| matches!(t.object, PTerm::Literal(_))).collect()
    }

    fn find_typed(&self) -> Option<&PTriple<A>> {
        self.vec.iter().find(
            |et| et.is_type()
        )
    }
}


// A set of terms that should be rendered as a RDF list, using first
// as a the subject of the first node
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct PTripleSeq<A:AsRef<str>> {
    list_seq: VecDeque<(PSubject<A>, Option<PTriple<A>>, PTriple<A>)>,
}

impl<A:AsRef<str> + Eq> From<PTripleSeq<A>> for Vec<PMultiTriple<A>> {
    fn from(seq: PTripleSeq<A>) -> Self {
        let mut v = vec![];
        for tup in seq.list_seq {
            let mut items = vec![];
            if let Some(t) = tup.1 {
                items.push(t);
            }
            items.push(tup.2);
            v.push(PMultiTriple::new(items));
        }
        v
    }
}

impl<A:AsRef<str> + Clone> PTripleSeq<A> {
    #[allow(dead_code)]
    pub (crate) fn empty() -> PTripleSeq<A> {
        PTripleSeq{list_seq: VecDeque::new()}
    }

    pub fn from_end(t:PTriple<A>) -> PTripleSeq<A> {
        let mut seq = PTripleSeq{list_seq: vec![].into()};
        if let PSubject::BlankNode(_) = &t.subject {
            seq.list_seq.push_front((t.subject.clone(), None, t));
        } else {
            todo!("This shouldn't happen")
        }
        seq
    }

    pub fn has_literal(&self) -> bool {
        self.list_seq.iter().any (
            |(_, t, _)|
            matches!(t,
                     Some(
                         PTriple {
                             subject:_,
                             predicate:_,
                             object: PTerm::Literal(_)
                         }
                     )
            )
        )
    }
}

impl<A> TripleLike<A> for PTripleSeq<A>
where A: AsRef<str> + Clone + Debug + Eq + PartialEq
{
    fn accept(&mut self, t:PTriple<A>) -> Option<PTriple<A>> {
        if t.is_collection_first() {
            if let Some(pos) = self.list_seq.iter().position(
                |tup| &tup.0 == &t.subject
            ){
                if let Some(tuple) = self.list_seq.get_mut(pos){
                    (*tuple).1 = Some(t)
                }

                return None;
            }
        }

        if let PTerm::BlankNode(bn) = &t.object {
            if let &PSubject::BlankNode(ref snn) = self.subject() {
                if t.is_collection_rest() && snn == bn {
                    self.list_seq.push_front((t.subject.clone(), None, t));
                    return None;
                }
            }
        }

        Some(t)
    }

    fn subject(&self) -> &PSubject<A> {
        &self.list_seq[0].0
    }

    fn literal_objects(&self) -> Vec<&PTriple<A>> {
        vec![]
    }

    fn find_typed(&self) -> Option<&PTriple<A>> {
        None
    }
}

// All the different forms of RDF subgraph
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum PExpandedTriple<A:AsRef<str>> {
    PMultiTriple(PMultiTriple<A>),
    PTripleSeq(PTripleSeq<A>),
}

impl<A> From<PTriple<A>> for PMultiTriple<A>
where A: AsRef<str> + Clone + Debug + Eq + PartialEq
{
    fn from(t: PTriple<A>) -> Self {
        PMultiTriple{
            vec: vec![t]
        }
    }
}

impl<A> From<PTriple<A>> for PExpandedTriple<A>
where A: AsRef<str> + Clone + Debug + Eq + PartialEq
{
    fn from(t: PTriple<A>) -> Self {
        let t:PMultiTriple<A> = t.into();
        t.into()
    }
}

impl<A> From<PMultiTriple<A>> for PExpandedTriple<A>
where A: AsRef<str> + Clone + Debug + Eq + PartialEq
{
    fn from(t: PMultiTriple<A>) -> Self {
        PExpandedTriple::PMultiTriple(t)
    }
}

impl<A> From<PTripleSeq<A>> for PExpandedTriple<A>
where A: AsRef<str> + Clone + Debug + Eq + PartialEq
{
    fn from(t: PTripleSeq<A>) -> Self {
        PExpandedTriple::PTripleSeq(t)
    }
}

impl<A> TripleLike<A> for PExpandedTriple<A>
where A: AsRef<str> + Clone + Debug + Eq + PartialEq {
    fn accept(&mut self, triple: PTriple<A>) -> Option<PTriple<A>> {
        match self {
            Self::PMultiTriple(mt) => mt.accept(triple),
            Self::PTripleSeq(seq) => seq.accept(triple),
        }
    }

    fn subject(&self) -> &PSubject<A> {
        match self {
            Self::PMultiTriple(mt) => mt.subject(),
            Self::PTripleSeq(seq) => seq.subject(),
        }
    }

    fn literal_objects(&self) -> Vec<&PTriple<A>> {
        match self {
            Self::PMultiTriple(mt) => mt.literal_objects(),
            Self::PTripleSeq(seq) => seq.literal_objects(),
        }
    }

    fn find_typed(&self) -> Option<&PTriple<A>> {
        match self {
            Self::PMultiTriple(mt) => mt.find_typed(),
            Self::PTripleSeq(seq) => seq.find_typed(),
        }
    }
}



/// A chunk of RDF that should that should be coherent.
/// Current invariants:
///   - each subject should appear only once (i.e. all subjects are
///   grouped in PMultiTriple, or AsRefSeq)
///   - if a subject appears it represents all appearances of the node
///   as a subject in the document of which this is a chunk
///   - if BNodes appear as subjects, they appear after any
///   apperance as an object (TODO: Not implemented yet!)
#[derive(Debug)]
pub struct PChunk<A:AsRef<str>>{
    v: VecDeque<PExpandedTriple<A>>,
    r: HashSet<PExpandedTriple<A>>,
    by_sub: HashMap<PBlankNode<A>, PExpandedTriple<A>>
}


impl<A> PChunk<A>
where A: AsRef<str> + Clone + Debug + Eq + Hash + PartialEq
{
    pub fn normalize(v:Vec<PTriple<A>>) -> Self {
        let mut etv:IndexMap<PSubject<A>, PMultiTriple<A>> = Default::default();
        let mut seq:Vec<PTripleSeq<A>> = vec![];
        let mut seq_rest:HashMap<PSubject<A>, PTriple<A>> = Default::default();
        let mut seq_first:HashMap<PSubject<A>, PTriple<A>> = Default::default();

        'top: for t in v {

            // We have a collection add. Create a new seq and store it
            if t.is_collection_end() {
                seq.push(PTripleSeq::from_end(t));
                continue 'top;
            }

            // We have a collection part. Remember for later
            if t.is_collection_rest() {
                if let PTerm::BlankNode(ref bn) = &t.object {
                    seq_rest.insert(PSubject::BlankNode(bn.clone()), t);
                }
                continue 'top;
            }
            if t.is_collection_first() {
                seq_first.insert(t.subject.clone(), t);
                continue 'top;
            }

            // We have something else. Combine it with existing multi
            // triples
            if let Some(multi) = etv.get_mut(&t.subject) {
                multi.accept(t);
            } else {
                // We have an orphan triple, store it a new multi
                etv.insert(t.subject.clone(), t.into());
            }
        }

        for s in seq.iter_mut() {
            loop {
                if let Some(t) = seq_first.remove(s.subject()) {
                    s.accept(t);
                }

                if let Some(t) = seq_rest.remove(s.subject()) {
                    s.accept(t);
                } else {
                    break;
                }
            }
        }

        let etv:VecDeque<PExpandedTriple<A>> = etv.into_iter()
            .map(|(_k, v)| v.into())
            .chain(
                seq.into_iter()
                    .map(|s| PExpandedTriple::PTripleSeq(s))
            )
            .collect();

        PChunk{v:etv, r:HashSet::new(), by_sub:HashMap::new()}
    }

    pub fn from_raw(vec:Vec<PExpandedTriple<A>>) -> Self {
        PChunk{v:vec.into(), r:HashSet::new(), by_sub:HashMap::new()}
    }

    pub fn empty() -> Self {
        PChunk{v:vec![].into(), r:HashSet::new(), by_sub:HashMap::new()}
    }

    pub fn sort(&mut self) {
        let _ = &self.v.make_contiguous()
            .sort_by(
                |a, b| {
                    match (a, b) {
                        (
                            PExpandedTriple::PMultiTriple(_),
                            PExpandedTriple::PTripleSeq(_)
                        ) => Ordering::Less,
                        (
                            PExpandedTriple::PTripleSeq(_),
                            PExpandedTriple::PMultiTriple(_)
                        ) => Ordering::Greater,
                        (
                            PExpandedTriple::PMultiTriple(
                                amt
                            ),
                            PExpandedTriple::PMultiTriple(
                                bmt
                            )
                        ) => {
                            match (
                                amt.subject(), bmt.subject()
                            ) {
                                (
                                    PSubject::NamedNode(_),
                                    PSubject::BlankNode(_),
                                ) => Ordering::Less,
                                (
                                    PSubject::BlankNode(_),
                                    PSubject::NamedNode(_),
                                ) => Ordering::Greater,
                                _ => Ordering::Equal
                            }
                        }
                        _ => Ordering::Equal
                    }
                }
            );
    }

    pub fn insert(&mut self, et:PExpandedTriple<A>){
        self.v.push_back(et);
        self.by_sub.clear()
    }

    pub fn next(&mut self) -> Option<PExpandedTriple<A>> {
        loop {
            if let Some(front) = self.v.pop_front() {
                if !self.r.contains(&front) {
                    return Some(front)
                }
            } else {
                return None;
            }
        }
    }

    fn remove_et(&mut self, et: &PExpandedTriple<A>) {
        self.r.insert(et.clone());
    }

    fn find_subject(&mut self, bn:&PBlankNode<A>) -> Option<PExpandedTriple<A>> {
        if self.by_sub.is_empty() {
            for v in self.v.iter() {
                if let PSubject::BlankNode(n) = v.subject() {
                    let _ = &self.by_sub.insert(n.clone(), v.clone());
                }
            }
        }
        if let Some(sub) = self.by_sub.get(bn).cloned() {
            if !self.r.contains(&sub) {
                return Some(sub);
            }
        }
        None
    }
}


#[derive(Clone, Debug, Default)]
pub struct ChunkedRdfXmlFormatterConfig {
    indent: usize,
    prefix: IndexMap<String, String>,
}

impl ChunkedRdfXmlFormatterConfig {
    pub fn none() -> Self {
        ChunkedRdfXmlFormatterConfig {
            indent: 0,
            prefix: IndexMap::new(),
        }
    }
    pub fn all() -> Self {
        ChunkedRdfXmlFormatterConfig {
            indent:4,
            prefix:IndexMap::new(),
        }
    }

    pub fn prefix(mut self, indexmap:IndexMap<String, String>) -> Self {
        self.prefix = indexmap;
        self
    }

    pub fn indent(mut self, indent:usize) -> Self{
        self.indent = indent;
        self
    }
}

pub struct ChunkedRdfXmlFormatter<A:AsRef<str>, W: Write> {
    writer: Writer<W>,
    config: ChunkedRdfXmlFormatterConfig,
    pub (crate) open_tag_stack: Vec<Vec<u8>>,
    last_open_tag: Option<BytesStart<'static>>,
    chunk: PChunk<A>,
}

impl<A, W> ChunkedRdfXmlFormatter<A, W>
where A: AsRef<str> + Clone + Debug + Eq + Hash + PartialEq,
      W: Write,
{
    pub fn new(write: W, mut config: ChunkedRdfXmlFormatterConfig) -> Result<Self, io::Error> {

        config.prefix.insert("http://www.w3.org/1999/02/22-rdf-syntax-ns#".to_string(),
                             "rdf".to_string());

        Self {
            writer: Writer::new_with_indent(write, b' ', config.indent),
            config,
            open_tag_stack: Default::default(),
            last_open_tag: None,
            chunk: PChunk::empty(),
        }
        .write_declaration()
    }

    fn write_declaration(mut self) -> Result<Self, io::Error> {
         self.write_event(Event::Decl(BytesDecl::new(b"1.0", Some(b"UTF-8"), None)))
            .map_err(map_err)?;
        let mut rdf_open = BytesStart::borrowed_name(b"rdf:RDF");
        self.write_prefix(&mut rdf_open)?;
        self.write_event(Event::Start(rdf_open))
            .map_err(map_err)?;
        Ok(self)
    }

    fn write_prefix(&mut self, rdf_open: &mut BytesStart<'_>) -> Result<(), io::Error> {
        for i in &self.config.prefix {
            let ns = format!("xmlns:{}", &i.1);
            rdf_open.push_attribute((&ns[..],
                                     &i.0[..]));
        }

        Ok(())
    }

    fn write_complete_open(&mut self) -> Result<(), quick_xml::Error> {
        if let Some(bs) = self.last_open_tag.take() {
            self.writer.write_event(Event::Start(bs))?;
        }
        self.last_open_tag = None;
        Ok(())
    }

    // Write a single event here.
    fn write_event(&mut self, event: Event<'_>) -> Result<(), quick_xml::Error> {
        self.write_complete_open()?;

        // If this is a start event, capture it, and hold it till the
        // next event. If the next event is a cognate close, send a Empty.
        self.writer.write_event(event)
    }

    fn write_start(&mut self, event: Event<'_>) -> Result<(), quick_xml::Error> {
        self.write_complete_open()?;
        match event {
            Event::Start(bs) => {
                self.open_tag_stack.push(bs.name().to_vec());
                self.last_open_tag = Some(bs.to_owned());
            }
            _ => panic!("Only pass a start event to write start"),
        }
        Ok(())
    }

    fn write_close(&mut self) -> Result<(), io::Error> {
        let close = self.open_tag_stack.pop().ok_or(
            io::Error::new(io::ErrorKind::Other, "close when no close is available")
        ).unwrap();

        //  println!("\nwrite_close:");
        if let Some(empty) = self.last_open_tag.take() {
            self.write_event(Event::Empty(empty)).map_err(map_err)
        } else {
            self.write_event(Event::End(BytesEnd::owned(close))).map_err(map_err)
        }
    }

    fn bytes_start_iri<'a>(&mut self, nn:&'a PNamedNode<A>) -> BytesStart<'a> {
        let (iri_protocol_and_host, iri_qname) = nn.split_iri();
        if let Some(iri_ns_prefix) = &self.config.prefix.get(iri_protocol_and_host) {
            BytesStart::owned_name(
                format!("{}:{}", &iri_ns_prefix, &iri_qname)
            )
        } else {
            let mut bs = BytesStart::owned_name(iri_qname.as_bytes());
            bs.push_attribute(("xmlns", iri_protocol_and_host));
            bs
        }
    }

    fn format_head<'a, T:TripleLike<A> + Debug>(&mut self, triple_like:&'a T, _chunk:&PChunk<A>)
                                        -> Result<Vec<&'a PTriple<A>>, io::Error> {
        let mut triples_rendered = vec![];
        let mut description_open =
            match triple_like.find_typed() {
                Some(t) => {
                    if let PTerm::NamedNode(nn) = &t.object {
                        triples_rendered.push(t);
                        self.bytes_start_iri(nn)
                    } else {
                        panic!("BNodes cannot be typed, I think")
                    }
                },
                None => {
                    BytesStart::borrowed_name(b"rdf:Description")
                }
            };

        match triple_like.subject() {
            PSubject::NamedNode(ref n) => {
                description_open.push_attribute(("rdf:about", n.iri.as_ref()))
            }
            PSubject::BlankNode(_) => {
                // Empty
            }
        }

        // TODO: Shares lots of code with format_property
        // TODO: check all properties unique!!
        for literal_t in triple_like.literal_objects() {
            if let PTerm::Literal(l) = &literal_t.object {
                match l {
                    PLiteral::Simple {value} => {
                        let (iri_protocol_and_host, iri_qname) = literal_t.predicate.split_iri();

                        if let Some(iri_ns_prefix) = &self.config.prefix.get(iri_protocol_and_host) {
                            description_open.push_attribute(
                                (
                                    &format!("{}:{}", &iri_ns_prefix, &iri_qname)[..],
                                    value.as_ref()
                                )
                            );
                            triples_rendered.push(literal_t);
                        }
                    }
                    PLiteral::LanguageTaggedString {value:_, language:_} => {
                        // Don't do anything here, because the
                        // language environment is wrong. Render later.
                    }
                    PLiteral::Typed {value:_, datatype:_} => {
                        // Don't do anything here because we need to
                        // render later.
                    }
                }
            } else {
                debug_assert!(false, "Non literal object returned from literal object method");
            }
        }
        self.write_start(Event::Start(description_open))
            .map_err(map_err)?;

        Ok(triples_rendered)
    }

    fn format_object(&mut self,
                     mut property_open:BytesStart<'_>,
                     object:&PTerm<A>, chunk:&mut PChunk<A>,
                     collection: bool
    )
                     -> Result<(), io::Error> {
        match object {
            PTerm::NamedNode(n) => {
                // Rewrite: 2.4 Empty Property Elements
                if collection {
                    property_open.push_attribute(("rdf:about", n.iri.as_ref()));
                } else {
                    property_open.push_attribute(("rdf:resource", n.iri.as_ref()));
                }

                self.write_start(Event::Start(property_open))
                    .map_err(map_err)?;
            }
            PTerm::BlankNode(n) => {
                if let Some(t) = chunk.find_subject(n) {
                    if let PExpandedTriple::PTripleSeq(ref seq) = t {
                        if !seq.has_literal() {
                            property_open.push_attribute(("rdf:parseType", "Collection"));
                        }
                    }
                    self.write_start(Event::Start(property_open))
                        .map_err(map_err)?;
                    self.format_expanded(&t, chunk)?;
                } else {
                    property_open.push_attribute(("rdf:nodeID", n.id.as_ref()));
                    self.write_start(Event::Start(property_open))
                        .map_err(map_err)?;
                }
            }
            PTerm::Literal(l) => {
                let content =
                    match l {
                        PLiteral::Simple { value } => {
                            property_open.push_attribute(("rdf:datatype",
                                                          "http://www.w3.org/2001/XMLSchema#string"
                            ));
                            value
                        }
                        PLiteral::LanguageTaggedString { value, language } => {
                            property_open.push_attribute(("xml:lang", language.as_ref()));
                            value
                        }
                        PLiteral::Typed { value, datatype } => {
                            property_open.push_attribute(("rdf:datatype", datatype.iri.as_ref()));
                            value
                        }
                    };
                self.write_start(Event::Start(property_open))
                    .map_err(map_err)?;
                self.write_event(Event::Text(BytesText::from_plain_str(&content.as_ref())))
                    .map_err(map_err)?;
            },
        };

        Ok(())
    }

    fn format_property_arc(&mut self, triple: &PTriple<A>,
                           rendered_in_head:&Vec<&PTriple<A>>,
                           chunk:&mut PChunk<A>,
    ) -> Result<(), io::Error> {
        if rendered_in_head.contains(&triple) {
            return Ok(())
        }

        let property_open = self.bytes_start_iri(&triple.predicate);
        self.format_object(property_open, &triple.object, chunk, false)?;

        self.write_close()?;
        Ok(())
    }

    fn format_seq(&mut self, seq: &PTripleSeq<A>, chunk:&mut PChunk<A>)
                  -> Result<(), io::Error> {

        // We can't format seqs with literals in like this -- we need
        // to do long hand
        if seq.has_literal() {
            let subj = seq.subject().clone();
            let v:Vec<PMultiTriple<A>> = seq.clone().into();
            for i in v {
                chunk.insert(i.into())
            }
            if let PSubject::BlankNode(n) = subj {
                return self.format_expanded(&chunk.find_subject(&n).unwrap(), chunk);
            }
            todo!("We shouldn't get here");
        }


        for tup in seq.list_seq.iter() {
            if let Some(ref triple) = tup.1 {
                match &triple.object {
                    // Just render in place
                    PTerm::BlankNode(bn) => {
                        if let Some(t) = chunk.find_subject(bn) {
                            self.format_expanded(&t, chunk)?;
                        }
                    }
                    // render the object, but not the property which
                    // is the collection joiner
                    PTerm::NamedNode(_) => {
                        let property_open = BytesStart::borrowed_name(b"rdf:Description");
                        self.format_object(property_open, &triple.object, chunk, true)?;
                        self.write_close()?;
                    }
                    _ => todo!()
                }
            }
        }

        Ok(())
    }

    fn format_multi(&mut self, multi_triple: &PMultiTriple<A>,
                    chunk:&mut PChunk<A>,
    ) -> Result<(), io::Error> {
        let rendered_in_head = self.format_head(multi_triple, chunk)?;

        // Rewrite: 2.3 Multiple Property Elements
        for triple in multi_triple.vec.iter() {
            self.format_property_arc(triple, &rendered_in_head, chunk)?;
        }

        self.write_close()?;
        Ok(())
    }

    fn format_expanded(&mut self, expanded:&PExpandedTriple<A>,
                       chunk:&mut PChunk<A>,
    ) -> Result<(), io::Error> {
        chunk.remove_et(expanded);

        match expanded {
            PExpandedTriple::PMultiTriple(ref mt) => {
                self.format_multi(mt, chunk)?;
            }
            PExpandedTriple::PTripleSeq(ref seq) =>{
                self.format_seq(seq, chunk)?;
            }
        }

        Ok(())
    }

    pub fn chunk_seq(&mut self, seq:PTripleSeq<A>) {
        self.chunk.insert(seq.into())
    }

    pub fn chunk_triple(&mut self, triple:PTriple<A>) {
        self.chunk.insert(triple.into());
    }

    pub fn chunk_multi(&mut self, multi: PMultiTriple<A>) {
        self.chunk.insert(multi.into())
    }

    pub fn sort_chunk(&mut self) {
        self.chunk.sort()
    }

    pub fn finish_chunk(&mut self) -> Result<(), io::Error> {
        let mut chk = PChunk::empty();
        std::mem::swap(&mut self.chunk, &mut chk);
        self.format_chunk(chk)
    }

    pub fn format_chunk(&mut self, mut chunk:PChunk<A>) -> Result<(), io::Error> {
        loop {
            let optet = chunk.next();
            if let Some(et) = optet {
                self.format_expanded(&et, &mut chunk)?;
            } else {
                break;
            }
        }
        Ok(())
    }

    /// Finishes writing and returns the underlying `Write`
    pub fn finish(mut self) -> Result<W, io::Error> {
        while !self.open_tag_stack.is_empty() {
            self.write_close()?;
        }

        self.finish_chunk()?;

        self.write_event(Event::End(BytesEnd::borrowed(b"rdf:RDF")))
            .map_err(map_err)?;

        Ok(self.writer.into_inner())
    }
}


pub struct PrettyRdfXmlFormatter<A:AsRef<str>+Debug, W: Write> (
    ChunkedRdfXmlFormatter<A, W>,
    pub Vec<PTriple<A>>
);

impl<A, W> PrettyRdfXmlFormatter<A, W>
where A: AsRef<str> + Clone + Debug + Eq + Hash + PartialEq,
      W: Write,
{
    pub fn new(write: W, config: ChunkedRdfXmlFormatterConfig) -> Result<Self, io::Error> {
        Ok(
            PrettyRdfXmlFormatter(
                ChunkedRdfXmlFormatter::new(write, config)?,
                vec![]
            )
        )
    }

    pub fn format(&mut self, triple:PTriple<A>) -> Result<(), io::Error> {
        let _ = &self.1.push(triple);
        Ok(())
    }

    pub fn triples(&self) -> Vec<PTriple<A>> {
        self.1.clone()
    }

    pub fn finish(mut self) -> Result<W, io::Error> {
        let chk = PChunk::normalize(self.1);
        self.0.format_chunk(chk)?;
        self.0.finish()
    }
}





#[cfg(test)]
mod test {
    use indexmap::{IndexMap, indexmap};
    use pretty_assertions::assert_eq;
    use rio_api::parser::TriplesParser;
    use rio_turtle::TurtleError;

    use super::{PChunk, PBlankNode, PNamedNode, PTriple, PTripleSeq,
                ChunkedRdfXmlFormatter, ChunkedRdfXmlFormatterConfig};

    fn tnn () -> PTriple<String> {
        PTriple {
            subject: PNamedNode::new("http://example.com/s".to_string()).into(),
            predicate: PNamedNode::new("http://example.com/p".to_string()).into(),
            object: PNamedNode::new("http://example.com/o".to_string()).into()
        }
    }

    fn tnn1 () -> PTriple<String> {
        PTriple {
            subject: PNamedNode::new("http://example.com/s1".to_string()).into(),
            predicate: PNamedNode::new("http://example.com/p1".to_string()).into(),
            object: PNamedNode::new("http://example.com/o1".to_string()).into()
        }
    }

    fn bnn () -> PTriple<String> {
        PTriple {
            subject: PBlankNode::new("hello_id".to_string()).into(),
            predicate: PNamedNode::new("http://example.com/p".to_string()).into(),
            object: PNamedNode::new("http://example.com/o".to_string()).into()
        }
    }

    #[test]
    pub fn chunk_hello_world() {
        assert!(true)
    }

    #[test]
    pub fn simple_chunk() {
        let chk = PChunk::normalize(
            vec![
                tnn(),
            ]
        );

        assert_eq!(chk.v.len(), 1);
    }

    #[test]
    pub fn multi_chunk() {
        let chk = PChunk::normalize(
            vec![
                tnn(),
                tnn(),
                tnn(),
            ]
        );

        assert_eq!(chk.v.len(), 1);
    }

    #[test]
    pub fn multi_chunk_sort_stable() {
        let mut chk:PChunk<String>=PChunk::empty();
        chk.insert(tnn().into());
        chk.insert(tnn1().into());
        chk.sort();

        assert_eq!(chk.next(), Some(tnn().into()));
        assert_eq!(chk.next(), Some(tnn1().into()));

        let mut chk:PChunk<String>=PChunk::empty();
        chk.insert(tnn1().into());
        chk.insert(tnn().into());
        chk.sort();

        assert_eq!(chk.next(), Some(tnn1().into()));
        assert_eq!(chk.next(), Some(tnn().into()));
    }

    #[test]
    pub fn multi_chunk_sort() {
        let mut chk:PChunk<String>=PChunk::empty();

        chk.insert(PTripleSeq::empty().into());
        chk.insert(bnn().into());
        chk.insert(tnn().into());

        chk.sort();

        assert_eq!(chk.next(), Some(tnn().into()));
        assert_eq!(chk.next(), Some(bnn().into()));
        assert_eq!(chk.next(), Some(PTripleSeq::empty().into()));
    }

    fn spec_prefix() -> IndexMap<&'static str, &'static str> {
        indexmap![
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#" => "rdf",
            "http://purl.org/dc/elements/1.1/" => "dc",
            "http://example.org/stuff/1.0/" => "ex"
        ]
    }

    #[allow(dead_code)]
    fn from_nt(nt: &str) -> String {
        from_nt_prefix(nt, indexmap!("http://www.w3.org/1999/02/22-rdf-syntax-ns#" => "rdf"))
    }

    fn from_nt_prefix(nt: &str, prefix: IndexMap<&str, &str>) -> String {
        let mut source: Vec<PTriple<String>> = vec![];
        let _: Vec<Result<(), TurtleError>>
            = rio_turtle::NTriplesParser::new(nt.as_bytes()).into_iter(
                |rio_triple| {
                    source.push(rio_triple.into());
                    Ok(())
                }
            ).collect();


        let sink = vec![];

        let mut config = ChunkedRdfXmlFormatterConfig::all();
        config.prefix =
            prefix.into_iter().map(
                |(k, v)|
                (k.to_string(), v.to_string())
            ).collect();

        let mut f = ChunkedRdfXmlFormatter::new(sink,config).unwrap();
        let chk = PChunk::normalize(source);
        //dbg!(&chk);
        f.format_chunk(chk).unwrap();

        let w = f.finish().unwrap();
        let s = String::from_utf8(w).unwrap();
        println!("{}", s);
        s
    }

    fn nt_xml_roundtrip_prefix(nt: &str, xml: &str, prefix: IndexMap<&str, &str>){
        assert_eq!(
            from_nt_prefix(nt, prefix), xml
        );
    }

    #[allow(dead_code)]
    fn nt_xml_roundtrip(nt: &str, xml: &str) {
        assert_eq!(
            from_nt(nt), xml
        );
    }

    #[test]
    fn example4_single_triple() {
        nt_xml_roundtrip_prefix(
r###"<http://www.w3.org/TR/rdf-syntax-grammar> <http://purl.org/dc/elements/1.1/title> "RDF1.1 XML Syntax" .
"### ,
r###"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:ex="http://example.org/stuff/1.0/">
    <rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar" dc:title="RDF1.1 XML Syntax"/>
</rdf:RDF>"### ,
            spec_prefix()
        )
    }

    #[test]
    fn example4_multiple_property_elements(){
        nt_xml_roundtrip_prefix(
r###"<http://www.w3.org/TR/rdf-syntax-grammar> <http://purl.org/dc/elements/1.1/title> "RDF1.1 XML Syntax" .
<http://www.w3.org/TR/rdf-syntax-grammar> <http://example.org/stuff/1.0/editor> _:genid1 .
_:genid1 <http://example.org/stuff/1.0/fullName> "Dave Beckett" .
_:genid1 <http://example.org/stuff/1.0/homePage> <http://purl.org/net/dajobe/> ."### ,

r###"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:ex="http://example.org/stuff/1.0/">
    <rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar" dc:title="RDF1.1 XML Syntax">
        <ex:editor>
            <rdf:Description ex:fullName="Dave Beckett">
                <ex:homePage rdf:resource="http://purl.org/net/dajobe/"/>
            </rdf:Description>
        </ex:editor>
    </rdf:Description>
</rdf:RDF>"### ,
            spec_prefix()
        );
    }

    #[test]
    fn example14_typed_nodes() {
        nt_xml_roundtrip_prefix(
r###"<http://example.org/thing> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/stuff/1.0/Document> .
<http://example.org/thing> <http://purl.org/dc/elements/1.1/title> "A marvelous thing" ."### ,

r###"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:ex="http://example.org/stuff/1.0/">
    <ex:Document rdf:about="http://example.org/thing" dc:title="A marvelous thing"/>
</rdf:RDF>"###  ,
            spec_prefix()
        )
    }

    #[test]
    fn example19_collections() {
        nt_xml_roundtrip_prefix(
r###"_:genid1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> <http://example.org/banana> .
_:genid2 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> <http://example.org/apple> .
_:genid1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> _:genid2 .
_:genid3 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> <http://example.org/pear> .
_:genid2 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> _:genid3 .
_:genid3 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> <http://www.w3.org/1999/02/22-rdf-syntax-ns#nil> .
<http://example.org/basket> <http://example.org/stuff/1.0/hasFruit> _:genid1 ."### ,

r###"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:ex="http://example.org/stuff/1.0/">
    <rdf:Description rdf:about="http://example.org/basket">
        <ex:hasFruit rdf:parseType="Collection">
            <rdf:Description rdf:about="http://example.org/banana"/>
            <rdf:Description rdf:about="http://example.org/apple"/>
            <rdf:Description rdf:about="http://example.org/pear"/>
        </ex:hasFruit>
    </rdf:Description>
</rdf:RDF>"### ,
            spec_prefix()
        )
    }
}

