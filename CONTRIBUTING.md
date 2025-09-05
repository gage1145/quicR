# Contributing to quicR

First off, thank you for considering contributing to **quicR**! Your feedback, ideas, and code help make this a better tool for the RT-QuIC research community.

## 🛠 Ways to Contribute

There are many ways you can contribute:

- **Bug reports**: If something doesn't work as expected, please [open an issue](https://github.com/gagerowden/quicR/issues).
- **Feature requests**: Have an idea for an enhancement? Let us know!
- **Pull requests**: Fix a bug, improve documentation, or add a feature.
- **Usage feedback**: Let us know how you're using `quicR` and what could be better.

## 🧪 Development Setup

To contribute code:

1. Fork the repository and clone it:
```bash
   git clone https://github.com/yourusername/quicR.git
   cd quicR
```

2. Install development dependencies in R and run the tests:
```R
install.packages("devtools")
devtools::load_all()
devtools::test()
```
3. Make you changes and commit with a clear message.
4. Push to your fork and open a pull request against the `main` branch.

## ✅ Pull Request Checklist

-   I have added or updated tests as needed.

-   I have documented any new functions.

-   I have run devtools::check() without errors.

## 📦 Style Guide
Follow [tidyverse style guide](https://style.tidyverse.org/) where possible. Consistency helps keep the code readable and maintainable.

## 📄 Code of Conduct

All contributors are expected to adhere to the [Contributor Covenant Code of Conduct](https://www.contributor-covenant.org/version/2/1/code_of_conduct/).

Thank you for helping make quicR better for the RT-QuIC community!