# A Simple, Statistically Robust Test of Discrimination.

Data and replication materials for Gaebler and Goel (2025)
["A Simple, Statistically Robust Test of
Discrimination"](https://doi.org/10.1073/pnas.2416348122).

## Citation

J.D. Gaebler, & S. Goel,   A simple, statistically robust test of
discrimination, _Proc. Natl. Acad. Sci. U.S.A._ 122 (10) e2416348122,
[https://doi.org/10.1073/pnas.2416348122](https://doi.org/10.1073/pnas.2416348122)
(2025).

## Instructions

To reproduce the analyses in our paper:

1. Make sure you have `R` version 4.4.0 and the `groundhog` package installed.
2. Ensure that you have `curl` installed, and run the download script from the
   root directory:
```bash
bash download.sh
```
3. Ensure you have `make` installed and run the following command from the root
   directory:
```bash
make clean
make
```

This will delete all of the processed output files for the COMPAS, LSAT, RIPA,
and SQF analyses, reprocess the raw data, and regenerate the plots in the
`plots` directory. The analysis and plotting take around half an hour on a
single M1 core.

The original download scripts used to obtain the data are contained in the root
directory: `download-compas.sh`, `download-lsat.sh`, `download-ripa.sh`, and
`download-sqf.sh`.
