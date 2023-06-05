module Download exposing (viewModel)
import Html exposing (..)
import Html.Attributes as HtmlAttr
import Html.Attributes exposing (..)
import Browser
import Dict
import Markdown
import View exposing (View)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row

viewModel : Html msg
viewModel =
    content

content: Html msg
content = 
    span [id "introduction"]
        [Markdown.toHtml [] """
# Data Downloads
We provide a 100% non-redundant catalog (964,970,496 smORFs) and a 90% amino-acid level catalog (287,926,875 smORFs families).

- The download files contain: 
  - protein / nucleotide fasta file
  - cluster table
  - taxonomy classification
  - habitat assignment
  - quality assessment
  - conserved domains annotation
  - cellular localization prediction
  - metadata

### Protein sequence (.fasta)
Fasta file of 100AA / 90AA protein sequences.

**100AA catalog:**&emsp;[GMSC10.100AA.faa.xz](https://gmgc.embl.de/download.cgi)

**90AA catalog:**&emsp;[GMSC10.90AA.faa.xz](https://gmgc.embl.de/download.cgi)

### Nucleotide sequence (.fasta)
Fasta file of 100AA / 90AA nucleotide sequences.

**100AA catalog:**&emsp;[GMSC10.100AA.fna.xz](https://gmgc.embl.de/download.cgi)

**90AA catalog:**&emsp;[GMSC10.90AA.fna.xz](https://gmgc.embl.de/download.cgi)

### Clusters
TSV table relating 100AA smORF accession and the hierarchically obtained clusters at 90% amino acid identity (which represent sequences with the same function).

Columns:

- `100AA smORF accession`
- `90AA smORF accession`

**Protein clustering table:**&emsp;[GMSC10.cluster.tsv.xz](https://gmgc.embl.de/download.cgi)

### Taxonomy classification
TSV table relating the taxonomy classification of 100AA / 90AA catalog.

Columns:

- `smORF accession`
- `taxonomy (separated by semicolon)`

**100AA catalog:**&emsp;[GMSC10.100AA.taxonomy.tsv.xz](https://gmgc.embl.de/download.cgi)

**90AA catalog:**&emsp;[GMSC10.90AA.taxonomy.tsv.xz](https://gmgc.embl.de/download.cgi)

### Habitat assignment
TSV table relating the habitat assignment of 100AA / 90AA catalog.

Columns:

- `smORF accession`
- `habitats (separated by comma)`


**100AA catalog:**&emsp;[GMSC10.100AA.habitat.tsv.xz](https://gmgc.embl.de/download.cgi)

**90AA catalog:**&emsp;[GMSC10.90AA.habitat.tsv.xz](https://gmgc.embl.de/download.cgi)

### Quality assessment
TSV table relating the high-quality sequences of 100AA / 90AA catalog.

Columns:

- `smORF accession`

**100AA catalog:**&emsp;[GMSC10.100AA.quality.tsv.xz](https://gmgc.embl.de/download.cgi)

**90AA catalog:**&emsp;[GMSC10.90AA.quality.tsv.xz](https://gmgc.embl.de/download.cgi)

### Conserved domains annotation
TSV table relating the Conserved domains annotation of 100AA / 90AA catalog.

Columns:

- `smORF accession`
- `identifier in CDD database (separated by comma)`

**100AA catalog:**&emsp;[GMSC10.100AA.cdd.tsv.xz](https://gmgc.embl.de/download.cgi)

**90AA catalog:**&emsp;[GMSC10.90AA.cdd.tsv.xz](https://gmgc.embl.de/download.cgi)

### Cellular localization prediction
TSV table relating the transmembrane or secreted sequences of 100AA / 90AA catalog.

Columns:

- `smORF accession`

**100AA catalog:**&emsp;[GMSC10.100AA.transmemrane-secreted.tsv.xz](https://gmgc.embl.de/download.cgi)

**90AA catalog:**&emsp;[GMSC10.90AA.transmemrane-secreted.tsv.xz](https://gmgc.embl.de/download.cgi)

### Metadata
TSV table relating the metadata of GMSC.

**Metadata:**&emsp;[GMSC10.metadata.tsv.xz](https://gmgc.embl.de/download.cgi)
"""]