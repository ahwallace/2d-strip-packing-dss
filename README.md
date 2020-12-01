# A Decision Support System for the Two-Dimensional Strip Packing Problem

This repository contains a computerised, user-friendly decision support system (DSS) for the two-dimensional strip packing problem. The work of this project is done by Aidan Wallace and Leslie Wu in partial fullment of the requirements for the degree of Honours in Statistics from the University of Cape Town.

## Two-dimensional strip packing problems (2D SPP)

The 2D SPP is fundamental to the class of cutting and packing problems. This problem is concerned with finding a suitable arrangement of a set of two-dimensional rectangular objects to be packed into a strip of fixed width and unlimited height in a non-overlapping manner, such that the height of the packed objects is minimised.

![Example of a 2D Strip Packing Problem](images/spp_example.PNG?raw=true "Title")

These problems have widespread practical relevance in industries such as logistics (e.g., cargo packing) and manufacturing (e.g., trim loss) where resource efficiency is essential. In these applications, the goals of each activity are directly related; to produce good quality arrangements of items so as to maximise material utilisation is equivalent to minimising wastage.

Strip packing is NP-hard! Numerous approaches to the strip packing problem have been proposed in the literature. The complex combinatorial nature of these problems means that choosing the right approach can be a difficult and time-consuming process. The decision making process can be substantially improved by providing simple, accurate and relevant comparisons of different approaches to aid the decision making process.

The literature on methods for solving the 2D SPP is extensive, and algorithms are generally classified into the classes of *exact methods*, *heuristic algorithms* or *approximate techniques*, and *hybrid metaheuristic algorithms*. The work of this project is centered in the latter two.

## Heuristic Algorithms

Heuristic algorithms are simple rule-based packing algorithms that allow users to quickly obtain good quality (although not necessarily optimal) solutions to packing problems. The DSS implements the methods of an improved best-fit method of Wei et al., and a constructive heuristic of Leung et al.

Both the improved best-fit and constructive heuristics rely on a two-stage packing procedure whereby a suitable space is found and thereafter an item to fit into the space is chosen basd on the above mentioned scoring rules. The item with with the highest score for the chosen space will be placed. Both methods rely on the bottom-left criteria where an item will be placed in the lowest, leftmost available space. If no item can fit into the bottom-left space, the next lowest, left-most space is evauted until all items are packed. The selection of the rectangles are based on simple scoring rules in which rectangles fitting into the space are scored and the item with the largest score is selected - ties are resolved by the initial packing order of the list of rectangles.

![An example scoring rule of the constructive heuristic](images/spp_example.PNG?raw=true "Title")

![An example scoring rule of the improved best-fit heuristic](images/spp_example.PNG?raw=true "Title")

Items in the the Constructive and Improved Best-Fit heuristics are placed next to the tallest neighbour. That is, the item is placed on the side of the space with the largest height.

## Hybrid Metaheuristic Algorithms

Metaheuristic algorithms combine a heursitic algorithm with a search for the best initial order of items. The metaheuristic searhces a set of item permutations while the heuristic algorithm is used to decode the item permutations into packing solutions. The best packing solution of the heuristic algorithm given the initial ordering of items found by the metaheuristic is returned. We explore the methods of a hybrid genetic algorithm and a hybrid simulated annealing approach.

### Hybrid Genetic Algorithm ###
Is based on the process of natural selection where an initial population of solutions taking the form of packing permutations are evolved over time using the gentics-inspired operators of crossover and mutation.


### Hybrid Simulated Annealing ###
Is based on the physical process of annealing. In broad terms, the simulated annealing (SA) algorithm typically commences with an initialisation of the starting parameters – a random initial solution and a relatively largestarting temperature. At each iteration, a neighbour of the current solution is randomly generated by some suitably defined mechanism. The objective function is evaluated atthis new point and thereafter the decision to accept or reject this as the new solution is performed probabilistically. Whilst running through the SA algorithm iteratively the temperature is cooled.

## packR
The DSS, *packR*, is based on the above algorithmic impementation of the routines above. The back-end was developed in R and the interface Shiny. Thereafter, the *DesktopDeployR* framework of W. Lee Pang allowed for the application to be distributed as a standalone desktop application devoid of any dependencies.

##
*packR* is available to be cloned and downloaded. The software can be run by executing the "RUN_APP.bat" file in the main directory.

## References

Burke, E. K., Kendall, G., & Whitwell, G. (2004). A new placement heuristic for the orthogonal stock-cutting problem. Operations Research, 52(4), 655-671.

Hopper, E., & Turton, B. C. (2001). A review of the application of meta-heuristic algorithms to 2D strip packing problems. Artificial Intelligence Review, 16(4), 257-300.

Leung, S. C., Zhang, D., & Sim, K. M. (2011). A two-stage intelligent search algorithm for the two-dimensional strip packing problem. European Journal of Operational Research, 215(1), 57-69.

Rakotonirainy, R. G. (2018). Metaheuristic solution of the two-dimensional strip packing problem (Doctoral dissertation, Stellenbosch: Stellenbosch University).

Wei, L., Hu, Q., Leung, S. C., & Zhang, N. (2017). An improved skyline based heuristic for the 2D strip packing problem and its efficient implementation. Computers & Operations Research, 80, 113-127.
