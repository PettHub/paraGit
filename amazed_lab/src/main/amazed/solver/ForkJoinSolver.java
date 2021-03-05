package amazed.solver;

import amazed.maze.Maze;

import java.util.*;
import java.util.concurrent.ConcurrentSkipListSet;

/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */


public class ForkJoinSolver
    extends SequentialSolver
{
    static ConcurrentSkipListSet<Integer> visited = new ConcurrentSkipListSet<>();
    Stack<Integer> frontier;
    //static boolean start = true;
    Integer current;
    Integer player;

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze   the maze to be searched
     */
    public ForkJoinSolver(Maze maze)
    {
        super(maze);
    }

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze        the maze to be searched
     * @param forkAfter   the number of steps (visited nodes) after
     *                    which a parallel task is forked; if
     *                    <code>forkAfter &lt;= 0</code> the solver never
     *                    forks new tasks
     */
    public ForkJoinSolver(Maze maze, int forkAfter)
    {
        this(maze);
        this.forkAfter = forkAfter;
        frontier = new Stack<>();
        frontier.push(maze.start());
        predecessor = new HashMap<>();
        player = maze.newPlayer(frontier.peek());
    }

    private ForkJoinSolver(Maze maze, Integer start, Map<Integer, Integer> predecessor){
        this(maze);
        frontier = new Stack<>();
        frontier.push(start);
        player = maze.newPlayer(start);
        this.predecessor = new HashMap<>(predecessor);
    }

    private void preFork(){}
    private void preJoin(){}

    /**
     * Searches for and returns the path, as a list of node
     * identifiers, that goes from the start node to a goal node in
     * the maze. If such a path cannot be found (because there are no
     * goals, or all goals are unreacheable), the method returns
     * <code>null</code>.
     *
     * @return   the list of node identifiers from the start node to a
     *           goal node in the maze; <code>null</code> if such a path cannot
     *           be found.
     */
    @Override
    public List<Integer> compute()
    {
        return parallelSearch(new ArrayList<>());
    }


    private List<Integer> parallelSearch(List<ForkJoinSolver> forks)
    {
        current = frontier.pop();
        maze.move(player, current);
        visited.add(current);
        if (maze.hasGoal(current)) return MapToList.compute(predecessor, maze.start(), current); //if we encounter a goal, return
        Set<Integer> neighbours = maze.neighbors(current);
        neighbours.removeAll(visited); //filter out the neighbours which are visited
        frontier.addAll(neighbours); //add all the neighbours to the frontier
        while (!frontier.empty()) {
            if (frontier.size() == 1) { //if there is only 1 way to go
                predecessor.put(frontier.peek(), current);
                parallelSearch(forks);
            }
            ForkJoinSolver fork = new ForkJoinSolver(maze, frontier.pop(), new HashMap(predecessor));
            fork.fork();
            forks.add(fork);
        }
        for (ForkJoinSolver f : forks) {
            List<Integer> list = f.join();
            for (Integer i : list) {
                if (maze.hasGoal(i)) return list;
            }
        }
        return new ArrayList<>();
    }

    private static class MapToList {
        public static List<Integer> compute(Map<Integer, Integer> map, Integer start, Integer current){
            List<Integer> path = new ArrayList<>();
            if (current == null) throw new NullPointerException("current");
            if (start == null) throw new NullPointerException("start");

            while(!start.equals(current)){
                path.add(current);
                current = map.get(current);
                if(current == null)
                    return path;
            }
            Collections.reverse(path);
            return path;
        }

    /*
    private List<Integer> parallelSearch()
    {
        current = frontier.pop();
        maze.move(player, current);
        visited.add(current);
        if (maze.hasGoal(current)) return MapToList.compute(predecessor, maze.start(), current); //if we encounter a goal, return
        Set<Integer> neighbours = maze.neighbors(current);
        neighbours.removeAll(visited); //filter out the neighbours which are visited
        frontier.addAll(neighbours); //add all the neighbours to the frontier
        ForkJoinSolver f2 = null, f3 = null;
        switch (frontier.size()){
            //case 0:
            //    return null;
            case 3:
                f3 = new ForkJoinSolver(maze, frontier.pop(), new HashMap(predecessor)); //if there is more than 2
                f3.fork();
            case 2:
                f2 = new ForkJoinSolver(maze, frontier.pop(), new HashMap(predecessor)); //if there is more than 1
                f2.fork();
            case 1: //if there is only 1 way to go
                predecessor.put(frontier.peek(), current);
                parallelSearch();
        }
        if (f3 != null){
            List<Integer> list3 = f3.join();
            for (Integer i : list3){
                if (maze.hasGoal(i)) return list3;
            }
        }
        if (f2 != null){
            List<Integer> list2 = f2.join();
            for (Integer i : list2){
                if (maze.hasGoal(i)) return list2;
            }
        }
        List<Integer> list1 = MapToList.compute(predecessor, maze.start(), current);
        for (Integer i : list1){
            if (maze.hasGoal(i)) return list1;
        }
        return null;
    }*/
}
