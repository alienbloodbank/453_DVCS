# 453_DVCS

### Group Discussion
see the `.pdf` and `.png` files in the directory Group Discussion; it includes new module guide, complete module specification, use hierarchy. 

## Team Manager - Max Wasserman
![Max Wasserman](https://www.cs.rochester.edu/people/graduate/assets/images/wasserman_max.jpg?1575253267788)

### Module Delegation
- TianCheng Xu: Commit, Pull, Push, Diff in Functionality Module
- Andrew Sexton: Status, Heads, Log, Checkout, Cat in Functionality Module
- Ziliang Lin: Repo(remote part), CommitConcept and MetaOrganization in SoftwareDecision/Concept, DvcsInterface in SoftwareDecision/Utility
- Soubhik Ghosh: Trackset, Repo in SoftwareDecision/Concept, Communication in SoftwareDecision,
                 Add, Remove, Clone, Init in Functionality Module

### Build Instructions:
        - make (Fire "make" command)

### Output
        - dvcs (binary file)

### Development
        Changing any haskell source doesn't sometimes allow make to rebuild for some reason
        In that case run 'make clean' and then run 'make' 

### Running Examples:
        - ./dvcs init
        - ./dvcs add <file1>
        - ./dvcs remove <file1> <file2>
        - ./dvcs status
        - ./dvcs clone <path>
        
      
