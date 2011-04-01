# Learning Chess Engine #
## CS51 Project Functional Specification ##

Identification: 
    * Han He : hanhe333@gmail.com
    * Robert Hero: robertmhero@gmail.com
    * David Palmer: dpalmer93@gmail.com
    * Amy Yin: amymichelleyin@gmail.com

### Brief Overview

We aim to implement the game of chess and develop two artificial intelligence chess engines.  We will first implement one engine based on a search tree and a standard evaluator function.  Then we plan to compare this engine to our “experimental” engines that will “learn” how to play chess.

The challenge of developing chess engines is that as the depth of brute-force search increases, the number of possible moves to evaluate increases exponentially, making it impossible to analyze all outcomes. An effective chess engine must handle this mass of data efficiently so that it runs within the time frame of an average chess move and nonetheless plays well against a human opponent. One solution to this problem is to use heuristics to cut down the number of positions that must be evaluated.  However, inventing these heuristics requires extensive knowledge of chess strategy. Instead, we aim to teach our engine its heuristics using two machine learning systems: neural networks and genetic algorithms. Our goal is to create a functional system that behaves at least at the level of the average human being. Ideally, we would like our program to be able to defeat human players.
