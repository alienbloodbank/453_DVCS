# 453_DVCS

### Group Discussion
see the `.pdf` and `.png` files in the directory Group Discussion; it includes new module guide, complete module specification, use hierarchy. 

## Team Manager - Max Wasserman
![Max Wasserman](https://www.cs.rochester.edu/people/graduate/assets/images/wasserman_max.jpg?1575253267788)

### Module Delegation
- TianCheng Xu: Commit, Pull, Push in Functionality Module
- Andrew Sexton: Heads, Log, Checkout, Cat in Functionality Module
- Ziliang Lin: Repo(remote part), CommitConcept and MetaOrganization in SoftwareDecision/Concept, DvcsInterface in SoftwareDecision/Utility
- Soubhik Ghosh: Trackset, Repo in SoftwareDecision/Concept, Communication in SoftwareDecision,
                 Add, Remove, Clone, Init, Status, Diff in Functionality Module

### Install Dependencies
        - make install

### Build Instructions:
        - make (Fire "make" command)

### Output
        - dvcs (binary file)

### Test
        - make test

### Development
        Changing any haskell source doesn't sometimes allow make to rebuild for some reason
        In that case run 'make clean' and then run 'make'.
        Running 'make clean' also removes the .dvcs folder.

### Running Examples:
        (Remember to give absolute path to the dvcs binary if testing in a different folder)
        - ./dvcs init
        - ./dvcs add <file> # Only adds text files!
        - ./dvcs remove <file>
        - ./dvcs status
        - ./dvcs clone <path>
        - ./dvcs commit <message>
        - ./dvcs heads
        - ./dvcs log
        - ./dvcs diff <commit_id1> <commit_id2>
        - ./dvcs cat <commit_id> <file>
        - ./dvcs checkout <commit_id>
        - ./dvcs pull <path>
        - ./dvcs push <path>
        
      
