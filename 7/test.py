# Python3 program to check whether a
# given array represents a max-heap or not

# Returns true if arr[i..n-1]
# represents a max-heap
def isHeap(arr, i, n):

# If a leaf node
    if i >= int((n - 2) / 2):
        return True

    # If an internal node and is greater
    # than its children, and same is
    # recursively true for the children
    if(arr[i] <= arr[2 * i + 1] and
       arr[i] <= arr[2 * i + 2] and
       isHeap(arr, 2 * i + 1, n) and
       isHeap(arr, 2 * i + 2, n)):
        return True

    return False

# Driver Code
if __name__ == '__main__':
    arr = [46,55,60,56,90,67,78,76,96,97,94,71,99,74,98,86]
    n = len(arr) - 1

    if isHeap(arr, 0, n):
        print("Yes")
    else:
        print("No")

    arr = [46,55,60,56,90,67,78,76,92,97,94,71,99,74,98,86,96]
    n = len(arr) - 1

    if isHeap(arr, 0, n):
        print("Yes")
    else:
        print("No")

# This code is contributed by PranchalK
