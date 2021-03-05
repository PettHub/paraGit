package amazed.solver;

import java.util.*;

public class MapToList {
    public static List<Integer> compute(Map<Integer, Integer> map, Integer start, Integer current){
        List<Integer> path = new ArrayList<>();
        if (current == null) throw new NullPointerException("current");
        if (start == null) throw new NullPointerException("start");
        while(!start.equals(current)){
            path.add(current);
            current = map.get(current);
        }
        Collections.reverse(path);
        return path;
    }
}
