module Help exposing (viewModel)

import Html exposing (Html, span)
import Html.Attributes as HtmlAttr
import Markdown

viewModel : Html msg
viewModel =
    content

content : Html msg
content =
    span [HtmlAttr.id "help"]
        [Markdown.toHtml [] """
## Overview
The global microbial smORFs catalogue (GMSC) is an integrated, consistently-processed, smORFs catalogue of the microbial world, combining 63,410 publicly available metagenomes from the [SPIRE database](http://spire.embl.de) and 87,920 high-quality isolated microbial genomes from the [ProGenomes2 database](https://progenomes.embl.de/).

A total of 4.5 billion smORFs were predicted to build the catalogue. After removing redundancy with 100% amino acid identity, we obtained a 100AA non-redundant catalogue with 964,970,496 sequences. Further, the smORFs were clustered at 90% amino acid identity resulting in 287,926,875 90AA smORFs catalogue.

In GMSC, `100AA` and `90AA` refer to catalogue identity thresholds rather than peptide length. `100AA` is the non-redundant catalogue after collapsing exact amino acid duplicates, while `90AA` groups related smORFs into family-level clusters at 90% amino acid identity.

## Citation

For more details about the GMSC, please see:

> Duan, Y., Santos-Júnior, C. D., Schmidt, T. S., Fullam, A., de Almeida, B. L. S., Zhu, C., Kuhn, M., Zhao, X.-M., Bork, P. & Coelho, L. P. A catalog of small proteins from the global microbiome. *Nature Communications* **15**, 7563 (2024). [DOI:10.1038/s41467-024-51894-6](https://doi.org/10.1038/s41467-024-51894-6)

Additionally, if you use GMSC in your research, please cite the above paper.

## Benefits and Features
**Integration:**

- GMSC is available as a web resource that displays each smORFs for browsing their integrated annotations:
  - clusters
  - taxonomy classification
  - habitat assignment
  - quality assessment
  - conserved domain annotation
  - cellular localization prediction

**Main purpose of GMSC**:
- Expand smORFs sets of global microbiomes with comprehensive annotation
- Analyse of ecological distribution patterns across taxonomy and global habitats
- Annotate smORFs of microbial genomes or genes with the resource

## Searching

##### Search by identifier
smORFs in the catalogue are identified with the scheme `GMSC10.100AA.XXX_XXX_XXX` or `GMSC10.90AA.XXX_XXX_XXX`. The initial `GMSC10` indicates the version of the catalogue (Global Microbial smORFs Catalogue 1.0). The `100AA` or `90AA` indicates the amino acid identity of the catalogue. The `XXX_XXX_XXX` is a unique numerical identifier (starting at zero). Numbers were assigned in order of increasing number of copies. So that the greater the number, the greater number of copies of that peptide were present in the raw data. 

On the 100AA Sequence page, the following information is displayed for each non-redundant smORF accession.

- Protein sequence
- Nucleotide sequence
- Taxonomic assignment
- Habitat
- Protein cluster
- Quality

On the 90AA Cluster page, the following information is displayed for each family-level cluster. The 100AA members of the cluster can be displayed by pressing the `show` button. 

- Consensus protein sequence
- Consensus nucleotide sequence
- Taxonomic assignment
- Habitat
- Number of 100AA smORFs
- Quality

##### Find homologues by sequence (GMSC-mapper)

GMSC-mapper is provided as a search tool for querying sequences. Users can provide contigs or protein sequences, and it will return a set of smORFs with complete annotations that match the 90AA smORF families in GMSC.

The search will take ~15 minutes. A search ID will be provided for each query. Search IDs are of the form `#-xxxx`, where `#` is an incrementing index and `xxxx` is a random string. 

Users can wait for results on the Mapper page or look them up later from the Home page using the search ID.

GMSC-mapper can also be downloaded and run locally; see details on the [GitHub page](https://github.com/BigDataBiology/GMSC-mapper).

![GMSC-mapper](assets/help_tool.png)

## Browse
Users can browse by habitats and taxonomy. For example, searching for `marine` will match entries such as `freshwater,marine,human gut`. Multiple habitats can be selected.

The results are 90AA smORF families spanning the selected habitats and taxonomy. Each row represents a family-level cluster rather than an individual non-redundant 100AA sequence.


## Data acquisition
63,410 assembled metagenomes were used from the [SPIRE database](http://spire.embl.de)

87,920 high-quality microbial genomes were downloaded from the [ProGenomes v2 database](https://progenomes.embl.de/).

## Construction of GMSC
![GMSC-mapper](assets/help_overview.png)
##### Prediction of smORFs
- We used a modified version of [Prodigal](https://github.com/hyattpd/Prodigal) to predict ORFs &gt;= 15 bps.  The ORFs that &lt;= 300 bps were considered smORFs.

##### Cluster generation
All predicted smORFs were removed redundancy with 100% amino acid identity. Then they were clustered with 90% amino acid identity and 90% coverage using [Linclust](https://github.com/soedinglab/MMseqs2). As a result, the 100AA catalogue stores non-redundant sequences, while the 90AA catalogue stores family-level clusters.

##### Taxonomy & Habitat annotation
- **Taxonomy annotation:**
  - The taxonomy of assembled contigs encoding the small proteins was annotated using [MMSeqs2](https://github.com/soedinglab/MMseqs2) against the [GTDB database](https://gtdb.ecogenomic.org/). 
  - We recorded the taxonomy of smORFs based on the taxonomy of the contigs of metagenomes or genomes of [Progenome v2 database](https://progenomes.embl.de/) from which the smORFs were predicted. Subsequently, we assign taxonomy for 100AA and 90AA smORFs using the lowest common ancestor (LCA), ignoring the un-assigned rank to make it more specific.
- **Habitat annotation:**
  - We recorded the habitats of smORFs according to their source samples using the habitat microontology introduced in the [SPIRE database](http://spire.embl.de). We further grouped the habitats into 8 broad categories: mammal gut, anthropogenic, other-human, other-animal, aquatic, human gut, soil/plant, and other.

##### Conserved domain annotation
The representative sequences of 90AA smORF families were searched against the [NCBI CDD database](https://www.ncbi.nlm.nih.gov/cdd/) by RPS-blast. Hits with a maximum e-value of 0.01 and at least 80% coverage of the PSSM length were retained and considered significant.

##### Cellular localization prediction
- [TMHMM2](https://services.healthtech.dtu.dk/services/TMHMM-2.0/) was run on the representative sequences of 90AA smORF families. 
- [SignalP-5.0](https://services.healthtech.dtu.dk/services/SignalP-5.0/) was run on the representative sequences of 90AA smORF families in `-org gram+`, `-org gram-`, and `-org arch` modes.

##### Quality assessment
- **Terminal check:** We checked for the presence of an in-frame STOP upstream of smORFs. As a smORF predicted at the start of a contig that is not preceded by an in-frame STOP codon risks being a false positive originating from an interrupted fragment.
- **Antifam:** We used HMMSearch to search smORFs against the [Antifam database 7.0](https://github.com/ebi-pf-team/antifam) to avoid spurious smORFs
- **RNAcode:** We used [RNAcode](https://github.com/ViennaRNA/RNAcode), a tool to predict the coding potential of sequences based on evolutionary signatures, to identify the coding potential of 90AA smORF families containing > 8 sequences. 
- **Metatranscriptomes:** We downloaded 221 sets of publicly available metatranscriptome data from the NCBI database paired with the metagenomic samples we used in our catalogue. We mapped reads against the representative smORFs of 90AA families by [BWA](https://github.com/lh3/bwa). The smORFs are considered to have transcriptional evidence only if they are mapped by reads across at least 2 samples. 
- **Ribo-Seq:** We downloaded 142 publicly available Ribo-Seq sets from the NCBI database.  We mapped reads against the representative smORFs of 90AA families by [BWA](https://github.com/lh3/bwa). The smORFs are considered to have translation evidence only if they are mapped by reads across at least 2 samples. 
- **Metaproteomes:** We downloaded peptide datasets from 108 metaproteome projects of the [PRIDE database](https://www.ebi.ac.uk/pride/). We exactly matched 100AA smORFs to the identified peptides of each project. If the total k-mer coverage of peptides on a smORF is greater than 50%, then the smORF is considered translated and detected.
""" ]
