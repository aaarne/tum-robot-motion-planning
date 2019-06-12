# Robot Motion Planning Exercises
Exercises for the Robot Motion Planning course in summer term 2019 at TUM.
Besides the homework for robot motion planning this project is intended to evaluate the [breeze](https://github.com/scalanlp/breeze) library
for scala. As well as the awesome [scalatest](http://www.scalatest.org/) testing framework.

# How to run
Assuming the scala build tool (sbt) is installed:
```bash
sbt run
```

Or create a distribution using
```
sbt stage
```
and run
```
target/universal/stage/bin/rmp
```

# Example Runs
## 1.1 Configuration Space

- __World:__
![World](ex/cs_world.png)

- __Configuration Space:__
![Configuration Space](ex/cs.png)

## 1.2 Visibility Graph
![Visibility Graph](ex/visibility_graph.png)

## 1.3 Voronoi Diagram
- __World:__
![World](ex/voronoi_world.png)
- __Voronoi Diagram:__
![Voronoi](ex/voronoi.png)

## 2.1 Probabilistic Roadmap (PRM) Pathplanning
The PRM is not connected. No solution found for the black query.
![PRM](ex/prm.png)

## 2.2 Rapidly-exploring Random Trees (RRT) Pathplanning (single tree mode)
![RRT](ex/rrt_single.png)
