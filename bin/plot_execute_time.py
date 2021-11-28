import matplotlib.pyplot as plt
import numpy as np
import os

data_dir = "data"
figure_path = "figure"

def read_data(path = "data2.txt"):
    x_list = []
    y_list = []
    with open(os.path.join(data_dir, path)) as f:
        lines = f.readlines()
        for line in lines:
            x, y = line.split(" ")
            x_list.append(float(x))
            y_list.append(float(y))
    return np.array(x_list), np.array(y_list)

# print(read_data()[1])

def plot_data(x, y, name, nb = True):
    plt.figure()
    plt.title(name)
    plt.ylabel("Time execution")
    if nb:
        plt.xlabel("Number of polynome")
    else:
        plt.xlabel("Length of polynome")
    plt.plot(x, y)
    plt.savefig(os.path.join(figure_path, name + ".png"))

def main():
    data_files = ["data" + str(i+1) + ".txt" for i in range(12)]
    for i, data_file in enumerate(data_files):
        x, y = read_data(data_file)
        if i < 3:
            plot_data(x, y, "Test on changing number of polynome. Strategie " + str(i % 3 + 1) + " for adding")
        elif i < 6:
            plot_data(x, y, "Test on changing number of polynome. Strategie " + str(i % 3 + 1) + " for multiplying")
        elif i < 9:
            plot_data(x, y, "Test on changing length of polynome. Strategie " + str(i % 3 + 1) + " for adding", nb=False)
        else:
            plot_data(x, y, "Test on changing length of polynome. Strategie " + str(i % 3 + 1) + " for multiplying", nb=False)


if __name__ == "__main__":
    main()