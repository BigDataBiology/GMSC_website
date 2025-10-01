module Download exposing (viewModel)
import Html
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
    Html.span [HtmlAttr.id "introduction"]
        [Markdown.toHtml [] """
# Data Downloads

See also the [Zenodo repository](https://zenodo.org/records/11206513) for files and details.

We provide the 964,970,496 non-redundant smORFs catalogue at 100% amino acid identity and 287,926,875 smORF families at 90% amino acid identity.

- The download files contain:
  - protein / nucleotide fasta file
  - cluster table
  - annotation table
  - metadata

### Protein sequence (.fasta)
Fasta file of 100AA / 90AA protein sequences.

**100AA smORF catalogue:**&emsp;[GMSC10.100AA.faa.xz](https://gmsc-api.big-data-biology.org/files/GMSC10.100AA.faa.xz)

**90AA smORF families:**&emsp;[GMSC10.90AA.faa.xz](https://gmsc-api.big-data-biology.org/files/GMSC10.90AA.faa.xz)

### Nucleotide sequence (.fasta)
Fasta file of 100AA / 90AA nucleotide sequences.

**100AA smORF catalogue:**&emsp;[GMSC10.100AA.fna.xz](https://zenodo.org/records/7944371/files/GMSC10.100AA.fna.xz?download=1)

**90AA smORF families:**&emsp;[GMSC10.90AA.fna.xz](https://gmsc-api.big-data-biology.org/files/GMSC10.90AA.fna.xz)

### Clusters
TSV table relating 100AA smORF accession and the hierarchically obtained families at 90% amino acid identity (which represent sequences with the same function).

Columns:

- `100AA smORF accession`
- `90AA smORF accession`

**Protein clustering table:**&emsp;[GMSC10.cluster.tsv.xz](https://zenodo.org/records/7944371/files/GMSC10.cluster.sorted2.tsv.xz?download=1)

### Annotations

TSV table containing the annotations of 100AA smORF catalogue. The position in the file indicates which smORF the annotation refers to (e.g., the line with index 1,621 refers to GMSC10.100AA.000_001_621 — index starting at 0/GMSC10.100AA.000_000_000). Two column are:

Columns:

- `Habitat annotation`: separated by comma
- `Taxonomic annotation`: separated by semicolon

**100AA smORF catalogue:**&emsp;[GMSC10.100AA.annotation.tsv.xz](https://zenodo.org/records/11206513/files/GMSC10.100AA.annotation.tsv.xz?download=1)

TSV table containing the annotations of 90AA smORF families. The position in the file indicates which smORF the annotation refers to (e.g., the line with index 1,621 refers to GMSC10.90AA.000_001_621 — index starting at 0/GMSC10.90AA.000_000_000). The table contains five columns:

Columns:

- `Habitat annotation`: separated by comma
- `Taxonomic annotation`: separated by semicolon
- `Conserved domain annotation`: identifiers in CDD database, separated by comma
- `TMHMM prediction`: the number of predicted transmembrane helices and the topology predicted, separated by semicolon
- `SignalP prediction`

**90AA smORF families:**&emsp;[GMSC10.90AA.annotation.tsv.xz](https://zenodo.org/records/11206513/files/GMSC10.90AA.annotation.tsv.xz?download=1)

### Quality assessment
TSV table containing the quality assessment of 100AA smORF catalogue and 90AA smORF families. As above, the position in the file indicates which smORF the annotation refers to (e.g., the line with index 1,621 refers to GMSC10.100AA.000_001_621 / GMSC10.90AA.000_001_621 — index starting at 0). Six quality assessment metrics are provided:

Columns:

- `AntiFam`: 'T' represents that smORF does not belong to the Antifam family. 'F' is the opposite.
- `Terminal checking`: 'T' represents that the upstream of smORF contains an in-frame STOP codon to rule out the possibility that the smORF is part of a broken gene due to contig fragmentation. 'F' is the opposite. 'NA' represents the checking was not performed on smORFs from Progenomes2 database.
- `RNAcode`: P-value from RNAcode. 'NA' represents the checking was not performed on smORFs families (8 members) or no reports in the results.
- `MetaTranscriptome`: The number of samples that smORFs are mapped.
- `Ribo-Seq`: The number of samples that smORFs are mapped.
- `MetaProteome`: The total k-mer coverage of peptides on smORFs.

**100AA smORF catalogue:**&emsp;[GMSC10.100AA.quality_test.tsv.xz](https://zenodo.org/records/11206513/files/GMSC10.100AA.quality_test.tsv.xz?download=1)

**90AA smORF families:**&emsp;[GMSC10.90AA.quality_test.tsv.xz](https://zenodo.org/records/11206513/files/GMSC10.90AA.quality_test.tsv.xz?download=1)

### Metadata
TSV table relating the metadata of GMSC.

The position in the file indicates which line each row refers to

Columns:

- `samples`: separated by comma

**100AA smORF catalogue:**&emsp;[GMSC10.100AA.sample.tsv.xz](https://zenodo.org/records/11206513/files/GMSC10.100AA.sample.txt.xz?download=1)

TSV table relating to metadata of samples used in GMSC.

Columns:

- `sample_accession`
- `ena_ers_sample_id`
- `database`
- `access_status`
- `study`
- `study_accession`
- `publications`
- `collection_date`
- `aliases`
- `microontology`
- `environment_biome`
- `environment_feature`
- `environment_material`
- `geographic_location`
- `latitude`
- `longitude`
- `host_common_name`
- `host_scientific_name`
- `host_tax_id`
- `general_envo_name`
- `higher_environment`

**Metadata:**&emsp;[GMSC10.metadata.tsv](https://gmsc-api.big-data-biology.org/files/GMSC10.metadata.tsv)
"""]
