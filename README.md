Pretty RDF/XML
==============

This library allows writing of [XML
RDF](https://www.w3.org/TR/rdf12-xml/). It is similar to the
[Rio](https://github.com/oxigraph/rio) writer, however, unlike Rio, it
is aimed at producing a readable syntax by taking the various
shortcuts that the RDF specification provides for. So, for instance, this longer piece of RDF:


```
<rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar">
  <ex:editor>
    <rdf:Description>
      <ex:homePage>
        <rdf:Description rdf:about="http://purl.org/net/dajobe/">
        </rdf:Description>
      </ex:homePage>
    </rdf:Description>
  </ex:editor>
</rdf:Description>

<rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar">
  <ex:editor>
    <rdf:Description>
      <ex:fullName>Dave Beckett</ex:fullName>
    </rdf:Description>
  </ex:editor>
</rdf:Description>

<rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar">
  <dc:title>RDF 1.2 XML Syntax</dc:title>
</rdf:Description>
```

will be shrunk using multiple property elements to this:


```
<rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar">
  <ex:editor>
    <rdf:Description>
      <ex:homePage>
        <rdf:Description rdf:about="http://purl.org/net/dajobe/">
        </rdf:Description>
      </ex:homePage>
      <ex:fullName>Dave Beckett</ex:fullName>
    </rdf:Description>
  </ex:editor>
  <dc:title>RDF 1.2 XML Syntax</dc:title>
</rdf:Description>
```
