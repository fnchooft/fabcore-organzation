# XML and DocBook

I have always been interrested in docbook. Never had to time to really
get into it, however, for some legacy systems the documentation is still
in docbook and currently there are no plans to get away from it.

So; let us take the opertunity to get to know a bit more, jotting down 
so insight and snipplets which are used along the way.

## Entity resolution

One thing I always liked is templating, and the simplest forms are the
best. Before jumping into a full-fledged tool based on python or lua and
template engines such as mustache etc, there must be simpler ways of injecting
variables into XML-documents.

### xmllint to the rescue

Let us take the following template as example, co-located variables at the
top for easy adaptation:

```xml
<!DOCTYPE config [
    <!ENTITY HOST "example.com">
    <!ENTITY PORT "8080">
    <!ENTITY PROTOCOL "HTTPS">
]>
<config>
    <server>
        <address>http://&HOST;:&PORT;</address>
        <protocol>&PROTOCOL;</protocol>
    </server>
</config>
```
To expand it run:

```bash
$ xmllint --noent config-co.xml.in --dropdtd > config.xml
```

The result:

```xml
<?xml version="1.0"?>
<config>
    <server>
        <address>http://example.com:8080</address>
        <protocol>HTTPS</protocol>
    </server>
</config>
```



Using a seperate file: vars.env

```bash
<!ENTITY HOST "example.com">
<!ENTITY PORT "8080">
<!ENTITY PROTOCOL "HTTPS">
```

Given the following template:

```xml
<!DOCTYPE config SYSTEM "vars.env">
<config>
    <server>
        <address>http://&HOST;:&PORT;</address>
        <protocol>&PROTOCOL;</protocol>
    </server>
</config>
```

Invoking xmllint: 
```bash
xmllint --noent --loaddtd input-env.xml.in --dropdtd > input-env.xml
```

Results in:

```xml
<?xml version="1.0"?>
<config>
    <server>
        <address>http://example.com:8080</address>
        <protocol>HTTPS</protocol>
    </server>
</config>
```

#### Take aways

- Simple, no need for any libraries - other than xmllint.
- Standard, no need for sed-voodoo magic ( eventhough this also works!)
- Validation
  - The file must be valid syntax
  - Proper error-messages, for instance if vars.env does not have PORT-entitiy:
```
input-env.xml.in:4: parser error : Entity 'PORT' not defined
        <address>http://&HOST;:&PORT;</address>
```