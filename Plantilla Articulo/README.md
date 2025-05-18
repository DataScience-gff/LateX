# Quarto Thesis Template

My Quarto thesis template.

- IMRAD structure
- APA citation style

## Project Structure

```
.
├── _meta
│   └── THIS DIRECTORY CONTAINS FILES THAT RARELY CHANGE
├── _output
│   └── THIS DIRECTORY CONTAINS THE OUTPUT PDF
├── document
│   └── THIS DIRECTORY CONTAINS MOST OF YOUR WORK
├── images
│   └── THIS DIRECTORY CONTAINS IMAGES
├── _quarto.yml
│   └── THIS FILE IS WHERE YOU CONFIGURE QUARTO
├── main.qmd
│   └── THIS FILE BOOTSTRAPS THE ENTIRE DOCUMENT
└── references.bib
    └── THIS FILE CONTAINS THE REFERENCES
```

## Requirements

- [Quarto](https://quarto.org/docs/get-started)
- Fonts: **Arial** for titles and body text (like everything else, fonts can be changed in `_quarto.yml`)
- Optional: [entr](https://github.com/eradman/entr) (Linux only)

## Setup

The following setup was only tested on Linux, it might be different on a other OS.

If you don't have a [TeX](https://www.latex-project.org/get/#tex-distributions) installation present on your system, the following needs to be executed once after the installation of Quarto:

`quarto install tinytex`

## Run

Render the entire project:

`quarto render`

### Live Reload

Automatically rerender the entire project if any Markdown file changes
(requires *entr*).

`ls **/*.*md | entr quarto render`

## Output

The output file (by default a PDF) is in the `_output` directory.

## License

CC0 1.0 Universal (CC0 1.0) Public Domain
