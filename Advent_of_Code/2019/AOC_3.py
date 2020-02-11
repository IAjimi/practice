
# coding: utf-8

# In[ ]:


#!/usr/bin/env python
# coding: utf-8

def drawline(string1):
    
    # Split string into list
    string1 = string1.split(",")
    
    # Set resulting line
    line = []
    
    # Internal variables
    x = 0
    y = 0
    
    # For every element in list
    for s in string1:
        travel_length = int(s[1:]) #length of line follows one initial letter, + 1 for python
        
        if s[0] == "R":
            temp = [((x + i), y) for i in range(0, travel_length)] #write down x, y coordinates
            x = x + travel_length
        
        if s[0] == "U":
            temp = [(x, (y + i)) for i in range(0, travel_length)]
            y = y + travel_length
            
        if s[0] == "L":
            temp = [((x - i), y) for i in range(0, travel_length)]
            x = x - travel_length
        
        if s[0] == "D":
            temp = [(x, (y - i)) for i in range(0, travel_length)]
            y = y - travel_length

        # Update Line
        line = line + temp
        
    line = line + [(x, y)] #adds last position
    
    return line


def manhattan_dist(seq1, seq2):
    l1 = drawline(seq1)
    l2 = drawline(seq2)
    
    #dist = [abs(i[0]) + abs(i[1]) for i in l1 if i in l2]
    dist = [abs(i[0]) + abs(i[1]) for i in list(set(l1) & set(l2))] #faster than double for loop
    dist.remove(0) #remove point 0, 0
    
    return min(dist)

def drawline_steps(string1):
    
    # Split string into list
    string1 = string1.split(",")
    
    # Set resulting line, step dictionary
    line = []
    steps = {}
    
    # Internal variables
    x = 0
    y = 0
    n = 0
    
    # For every element in list
    for s in string1:
        travel_length = int(s[1:]) #length of line follows one initial letter, + 1 for python
        
        for i in range(0, travel_length):
            n += 1
            
            if s[0] == "R":
                temp = ((x + 1), y) #write down x, y coordinates
                x = x + 1
        
            if s[0] == "U":
                temp = (x, (y + 1))
                y = y + 1
            
            if s[0] == "L":
                temp = ((x - 1), y)
                x = x - 1
        
            if s[0] == "D":
                temp = (x, (y - 1))
                y = y - 1

            # Update Line
            if temp not in steps.keys(): #if spot hasn't been visited yet
                steps[temp] = n

    steps[(x, y)] = 1
    return steps

def manhattan_dist_steps(seq1, seq2):
    l1 = drawline_steps(seq1)
    l2 = drawline_steps(seq2)
    
    intersection = [l1[i] + l2[i] for i in list(set(l1.keys()) & set(l2.keys()))] #faster than double for loop
    
    return min(intersection)


if __name__ == "__main__":
    input_1 = str(input('First input: ')).replace('"', "") #paste in input 1
    input_2 = str(input('Second input: ')).replace('"', "")
    
    print("PART 1")
    print(manhattan_dist(input_1, input_2))
    
    print("PART 2")
    print(manhattan_dist_steps(input_1, input_2))

