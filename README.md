# hs-information-retrieval

Welcome to the Information Retrieval project repository! This project is written in Haskell and is part of the University Course "Information Retrieval."

## About

The aim of this project is to implement advanced algorithms and techniques for information retrieval. Through this repository, we explore the fascinating world of retrieving and analyzing information using Haskell's powerful and expressive features.

## Tasks:

### Task 1: Dictionary

- Write a program that builds a dictionary of terms based on a given collection of text files.
- Text files can be submitted in any format.
- The size of text files is at least 150K.
- The number of text files is at least 10.
- Save the glossary of terms to disk.
- Estimate the size of the collection, the total number of words in the collection, and the size of the dictionary.
- Justify the chosen data structure.
- Evaluate the complexity of the algorithm.
- Try several formats of saving the dictionary (dictionary serialization, saving to a text file, etc.) and compare the results.

### Task 2: Boolean Search

- Based on the given collection (10 documents of 150K) of documents, build:
  - "term-document" incidence matrix.
  - Inverted index.
- Justify the selected data storage structures in terms of their effectiveness when increasing data volumes.
- Compare the size of the resulting structures.
- Perform a Boolean search on these structures (on both).
- Operators: AND, OR, NOT. The format in the request is at your discretion.

### Task 3: Two-Word Index and Coordinate Inverted Index

- Build a two-word index and a coordinate inverted index on a collection of documents.
- Implement phrase search and distance-based search for each of them.

### Task 4: Advanced Indexing

- Build a suffix tree for the dictionary of terms.
- Build a permutation index for the dictionary.
- Build a 3-gram index for the dictionary.
- Implement wildcard search support for queries.

### Task 5: Implement Joker Queries

- Implement support for wildcard queries (queries with jokers) in the retrieval system.
- Allow users to search for terms using wildcard characters that represent unknown characters in the query.
- Implement efficient algorithms to handle wildcard queries and retrieve relevant results.

## How to Use

1. Move the FB2 format files to the `resources/fb2` folder in the project directory.

2. Open the console or terminal and navigate to the project directory.

3. Run the following commands:

    ```bash
    stack build
    stack exec hs-information-retrieval
    ```
