---
name: Bug report
about: Use this if the generator does not do what it should
title: ''
labels: bug
assignees: scymtym

---

**Which generator version are you using?**

```sh
build-generator version
# Add the output of the above command here
```

**What did you do?**

1. *(If applicable)* The relevant recipes look like this

   * `distributions/foo.distribution`:

     ```yaml
     versions:
       - bar
       …
     ```

   * `projects/bar.project`:

     ```yaml
     variables:
       …
     ```

2. Generator configuration and invocation
   *(If applicable)* Configuration file:
   ```ini
   # Insert contents effective configuration file or files, e.g. ${HOME}/.config/build-generator.conf
   ```
   Generator invocation and output:
   ```shell
   build-generator generate …
   # Depending on what seems relevant, insert complete output or only relevant parts
   ```

**Which behavior did you expect?**
A clear and concise description of what you expected to happen. Which output should the generator have produced? Which Jenkins job configuration should have been generated?

**What happened instead?**
Describe what happened instead of the expected behavior?

*If applicable, add screenshots of the generator output or Jenkins pages to help explain the problem.*
