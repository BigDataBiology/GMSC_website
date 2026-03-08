# GMSC Website

Website for the **Global Microbial smORFs Catalogue (GMSC)** — an integrated, consistently-processed catalogue of ~965 million small open reading frames (smORFs) from microbial metagenomes and high-quality isolated genomes.

Live website: [gmsc.big-data-biology.org](https://gmsc.big-data-biology.org/)

## Functionality

The website provides:

- identifier lookup for individual 100AA sequences and 90AA clusters
- sequence search through GMSC-mapper for protein or contig FASTA input
- browse and filtering by habitat, taxonomy, and quality criteria
- downloads, help, and project/about pages
- result views for sequence details, cluster members, and mapper hits

## Citation

> Duan, Y., Santos-Júnior, C. D., Schmidt, T. S., Fullam, A., de Almeida, B. L. S., Zhu, C., Kuhn, M., Zhao, X.-M., Bork, P. & Coelho, L. P. A catalog of small proteins from the global microbiome. *Nature Communications* **15**, 7563 (2024). https://doi.org/10.1038/s41467-024-51894-6

## Building

This is an [Elm](https://elm-lang.org/) 0.19.1 single-page application.

```bash
./build.sh
```

This compiles `src/Main.elm` into `dist/index.html` and copies static assets (`style.css`, `assets/`, `_redirects`) into `dist/`.

## Deployment

Deployed on [Netlify](https://www.netlify.com/). See `netlify.toml` for configuration.

## License

See [LICENSE](LICENSE).
