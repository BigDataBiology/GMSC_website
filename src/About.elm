module About exposing (viewModel)

import Html exposing (Html, span)
import Html.Attributes as HtmlAttr
import Markdown

viewModel : Html msg
viewModel =
    content

-- main text

content: Html msg
content =
    span [HtmlAttr.id "introduction"]
        [Markdown.toHtml [] """
## The global microbial smORF catalogue (GMSC)

**Citation:** Duan, Y., Santos-Júnior, C. D., Schmidt, T. S., Fullam, A., de Almeida, B. L. S., Zhu, C., Kuhn, M., Zhao, X.-M., Bork, P. & Coelho, L. P. A catalog of small proteins from the global microbiome. *Nature Communications* **15**, 7563 (2024). https://doi.org/10.1038/s41467-024-51894-6

For more information, see the [published article](https://www.nature.com/articles/s41467-024-51894-6).

GMSC was developed by the [Big Data Biology Research Group](https://www.big-data-biology.org/).
___
**Version:** &emsp;&ensp; 1.0
___
**Contacts:** &emsp; [yiqian@big-data-biology.org](mailto:yiqian@big-data-biology.org) and [luispedro@big-data-biology.org](mailto:luispedro@big-data-biology.org)
___
**Affiliations:** &ensp; Institute of Science and Technology for Brain-Inspired Intelligence ([ISTBI](https://istbi.fudan.edu.cn/)). Fudan University, Shanghai, China and [Centre for Microbiome Research](https://research.qut.edu.au/cmr/), School of Biomedical Sciences, Queensland University of Technology, Translational Research Institute, Woolloongabba, Queensland, Australia.
""" ]
