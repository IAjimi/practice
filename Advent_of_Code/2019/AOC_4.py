
# coding: utf-8

# In[60]:


def check_both_1(num):
    num = str(num)
    repete = False
    
    for i in range(0, len(num)):
        if i != len(num) - 1: # Not End of Range, since using i + 1 below
            if num[i] > num[i+1]: # Break if digits decrease
                return 
            if num[i] == num[i+1]: # Check for repetition
                repete = True
    
    if repete == True: # Return if loop wasn't broken (no digit decrease)
        return int(num) 
    
    
def check_both_2(num):
    num = str(num)
    repete = {str(k):0 for k in range(10)} # Dictionary with number of repetition per digit btw 0 - 9
    
    for i in range(0, len(num)):
        if i != len(num) - 1: 
            if num[i] > num[i+1]: 
                return 
            if num[i] == num[i+1]:
                repete[num[i]] += 1
    
    if 1 in repete.values(): # If one number is repeted twice in a row
        return int(num) 

if __name__ == "__main__":
    
    l = [i for i in range(367479,893698)] 
    
    list1 = list(map(check_both_1, l))
    list1 = [i for i in list1 if i != None] #remove none types
    print("PART 1:", len(list1))

          
    list1 = list(map(check_both_2, l)) 
    list1 = [i for i in list1 if i != None]
    print("PART 2:", len(list1))

