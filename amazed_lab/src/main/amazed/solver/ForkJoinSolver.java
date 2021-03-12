package amazed.solver;

import amazed.maze.Maze;

import java.util.*;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.atomic.AtomicInteger;

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
    Integer current;
    Integer player;
    static AtomicInteger counter = new AtomicInteger(0);
    int count = 0;
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

    private ForkJoinSolver(Maze maze, Integer start, Map<Integer, Integer> predecessor, int count){
        this(maze);
        frontier = new Stack<>();
        frontier.push(start);

        this.predecessor = new HashMap<>(predecessor);
        this.count = count;
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
        return parallelSearch(new HashSet<>());
    }


    private List<Integer> parallelSearch(HashSet<ForkJoinSolver> forks)
    {
        current = frontier.pop();
        boolean tmpVisit = visited.add(current);
        if(player == null && tmpVisit){
            player = maze.newPlayer(start);
        }
        else if(tmpVisit)
            maze.move(player, current);
        if (maze.hasGoal(current)){
            return MapToList.compute(predecessor, maze.start(), current); //if we encounter a goal, return path
        }
        Set<Integer> neighbours = maze.neighbors(current);
        neighbours.removeAll(visited); //filter out the neighbours which are visited
        frontier.addAll(neighbours); //add all the neighbours to the frontier
        while (!frontier.empty()) {
            if (frontier.size() == 1) { //if there is only 1 way to go
                predecessor.put(frontier.peek(), current);
                return parallelSearch(forks);
            }
            else{
                Map<Integer, Integer> tmpMap = new HashMap(predecessor);
                Integer tmpStart = frontier.pop();
                tmpMap.put(tmpStart, current);
                ForkJoinSolver fork = new ForkJoinSolver(maze, tmpStart, tmpMap, counter.incrementAndGet());
                fork.fork();
                forks.add(fork);
            }
        }
        for (ForkJoinSolver f : forks) {
            if(f.join() != null) {
                List<Integer> list = f.join();
                if (!list.isEmpty()) return list;
            }
        }
        return null;
    }

    private static class MapToList {
        public static List<Integer> compute(Map<Integer, Integer> map, Integer start, Integer current){
            List<Integer> path = new ArrayList<>();
            if (current == null) throw new NullPointerException("current");
            if (start == null) throw new NullPointerException("start");
            do{
                path.add(current);
                current = map.get(current);
                if(current == null)
                    return path;
            }while(!start.equals(current));
            path.add(start);

            Collections.reverse(path);
            return path;
        }
    }

}
