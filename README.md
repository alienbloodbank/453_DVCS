# 453_DVCS

Build Instructions:
        - cd src/ (Go inside source directory)
        - make (Fire "make" command)

Output
        - dvcs (binary file)

Development
        Changing any haskell source doesn't sometimes allow make to rebuild for some reason
        In that case run 'make clean' and the run 'make' 

Running Examples:
        - ./dvcs init
        - ./dvcs add <file1>
        - ./dvcs remove <file1> <file2>
        - ./dvcs status
        - ./dvcs clone <path>
