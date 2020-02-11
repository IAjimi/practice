
# coding: utf-8

# In[3]:


def fuel_calc(input_):
    fuel = input_ // 3 - 2
    return fuel

def fuel_compound(input_):
    fuel = fuel_calc(input_)
    sum_ = fuel
    
    while fuel >= 6: # i.e. fuel positive based on input_ // 3 - 2
        fuel = fuel_calc(fuel)
        sum_ += fuel
        
    return sum_

if __name__ == "__main__":
    file = open("day_1_input.txt", "r")
    l = [int(i.replace("\n", "")) for i in file]
    
    print("PART 1")
    sum(list(map(fuel_calc, l)))
    print("")
    print("PART 2")
    sum(list(map(fuel_compound, l)))

