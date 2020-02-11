
# coding: utf-8

# In[3]:


def new_intcode(list2, noun, verb):
    i = 0
    list1 = list2.copy()
    list1[1] = noun
    list1[2] = verb

    while list1[i] in [1, 2] and i <= len(list1):
        if list1[i] == 1:
            list1[list1[i+3]] = list1[list1[i+1]] + list1[list1[i+2]]
            i = i + 4
        
        if list1[i] == 2:
            list1[list1[i+3]] = list1[list1[i+1]] * list1[list1[i+2]]
            i = i + 4
    
    return list1[0]

if __name__ == "__main__":
    input_ = "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,2,9,19,23,2,13,23,27,1,6,27,31,2,6,31,35,2,13,35,39,1,39,10,43,2,43,13,47,1,9,47,51,1,51,13,55,1,55,13,59,2,59,13,63,1,63,6,67,2,6,67,71,1,5,71,75,2,6,75,79,1,5,79,83,2,83,6,87,1,5,87,91,1,6,91,95,2,95,6,99,1,5,99,103,1,6,103,107,1,107,2,111,1,111,5,0,99,2,14,0,0"
    my_list = input_.split(",")
    my_list = list(map(int, my_list))
    
    print("PART 1")
    print(new_intcode(my_list, 12, 2))
    print("")
    print("PART 2")
    for i in range(0, 100):
        for j in range(0, 100):
            if new_intcode(my_list, i, j) == 19690720:
                print(i, j)

