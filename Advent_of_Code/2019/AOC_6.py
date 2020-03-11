def number_orbits(nodes): #length of path from each node to origin (COM)
    dist = {} #empty dict of distances from each note to origin

    for i in set(nodes.keys()): 
        dist[i] = 1
        next_node = nodes[i] #unique node values -> get following node (move to left)
            
        while next_node in nodes.keys():
            dist[i] += 1
            next_node = nodes[next_node]
            
    return sum(dist.values())

def path_to_com(node): #creates list of nodes on the way to origin (COM)
    path_list = []
    next_node = nodes[node] #unique node values
    path_list.append(next_node)
        
    while next_node in nodes.keys():
        next_node = nodes[next_node]
        path_list.append(next_node)
    
    return path_list

def find_shortest_path(list1, list2):
    position = [l1.index(i) for i in list(set(l1) & set(l2))]  #find position of intersections in YOUr path
    steps_l1 = min(position) #steps required to get to earliest intersection -- starting point is 0
    steps_l2 = l2.index(l1[min(position)]) #steps required to get to earliest intersection
    
    return steps_l1 + steps_l2 

if __name__ == "__main__":
    file = open("adventofcode6.txt", "r")
    input_list = [i.replace("\n", "") for i in file] #remove newlines
    nodes = {k.split(")")[1]:k.split(")")[0] for k in input_list} #create dict that reads rel right to left

    print("RESULT OF PART 1: %s" % number_orbits(nodes))

    l1, l2 = path_to_com("YOU"), path_to_com("SAN") #get path from YOU to COM and from SAN to COM
    print("RESULT OF PART 2: %s" % find_shortest_path(l1, l2))