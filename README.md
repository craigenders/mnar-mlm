# Missing Not at Random Models for Multilevel Analyses of Longitudinal and Intensive Longitudinal Data

Online materials for Enders, Keller, and Laurenceau. Rendered documents are at
**https://craigenders.github.io/mnar-mlm/**.

## Documents

| Document | Description |
|---|---|
| [MNAR Tutorial](https://craigenders.github.io/mnar-mlm/MNAR-Tutorial.html) | Blimp and rblimp tutorial covering every model in the paper |
| [IRT Equivalence](https://craigenders.github.io/mnar-mlm/IRT-Equivalence.html) | Equivalence of dummy-coded and item response theory missingness models |
| [Blimp Installation Guide](https://craigenders.github.io/mnar-mlm/Blimp-Installation-Guide.html) | Installing Blimp and rblimp |
| [Blimp Scripting Quick Start](https://craigenders.github.io/mnar-mlm/Blimp-Scripting-Quick-Start.html) | Introduction to the Blimp scripting language |

## Repository layout

| Folder | Contents |
|---|---|
| `data/` | Four example data sets: `growth-dropout.csv`, `growth-intermittent.csv`, `intensive-dropout.csv`, `intensive-intermittent.csv` |
| `rblimp/` | R scripts that fit every model with rblimp (one per data set), `irt-missingness-models.R`, and `mnar-plotting.R` |
| `blimp/` | The same models as Blimp scripts |
| `docs/` | The rendered HTML documents listed above |

## Software

Blimp and rblimp are free and available at https://www.appliedmissingdata.com/blimp.
rblimp requires the Blimp application to be installed.
