import matplotlib.pyplot as plt
import numpy as np
import os

data_dir = "data"
figure_path = "figure"

def normalize_list(a):
    a = np.array(a)
    a = a / np.linalg.norm(a)

    return a

def read_data(path = "data2.txt"):
    x_list = []
    y_list = []
    with open(os.path.join(data_dir, path)) as f:
        lines = f.readlines()
        for line in lines:
            x, y = line.split(" ")
            x_list.append(float(x) / 1000)
            y_list.append(float(y))

    return x_list, y_list

# print(read_data()[1])

def plot_data(x, y, label, nb=True):
    plt.ylabel("Time execution")
    if nb:
        plt.xlabel("Number of polynome")
    else:
        plt.xlabel("Length of polynome")
    plt.plot(x, y, label=label)

def main():
    my_dpi = 96
    plt.figure(figsize=(800/my_dpi, 800/my_dpi), dpi=my_dpi)
    data_files = ["data" + str(i+1) + ".txt" for i in range(12)]
    for i, data_file in enumerate(data_files):
        x, y = read_data(data_file)
        if i < 3:
            plt.subplot(2, 2, 1)
            plot_data(x, y, "Strategie " + str(i % 3 + 1))
            if i == 2:
                plt.title("2.14")
                plt.legend()
        elif i < 6:
            plt.subplot(2, 2, 2)
            plot_data(x, y, "Strategie " + str(i % 3 + 1))
            if i == 5:
                plt.title("2.15")
                plt.legend()
        elif i < 9:
            plt.subplot(2, 2, 3)
            plot_data(x, y, "Strategie " + str(i % 3 + 1), False)
            if i == 8:
                plt.title("2.16")
                plt.legend()
        else:
            plt.subplot(2, 2, 4)
            plot_data(x, y, "Strategie " + str(i % 3 + 1), False)
            if i == 11:
                plt.title("2.17")
                plt.legend()

    plt.subplots_adjust(left=0.2,
                        bottom=0.1, 
                        right=0.8, 
                        top=0.9, 
                        wspace=0.4, 
                        hspace=0.4)
    plt.savefig(os.path.join(figure_path, "1.png"), dpi=my_dpi * 10)


if __name__ == "__main__":
    main()