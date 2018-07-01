# genetic-tsp
Approximating the travelling salesman problem with a genetic algorithm

## Dependencies
This project depends on the gloss package. To install simple run
`cabal install gloss`

## To Run

Compile the project with `ghc --make tsp.hs`

To run the code, execute `./tsp`

Variables in `tsp.hs` govern the number of generations

```haskell
generations = 1000
```

Variables in `Genetic.hs` govern the size of generations and how they evolve

```haskell
gensize = 20
nCross = 20
nMutate = 30
```

## Results

Example #1

![](assets/example1.png)

Example #2

![](assets/example2.png)
