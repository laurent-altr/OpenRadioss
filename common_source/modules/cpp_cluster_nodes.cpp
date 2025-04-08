#include <vector>
#include <cmath>
#include <algorithm>
#include <random>
#include <unordered_set>
#include <set>
#include <array>
#include <unordered_map>
#include <iostream>
#include <queue>

const int UNCLASSIFIED = -1;
const size_t MAX_CELLS = 32;

template <typename T>
const T &clamp(const T &value, const T &min, const T &max)
{
    return (value < min) ? min : ((value > max) ? max : value);
}

// maps a 3D coordinate to a 1D index
size_t map3Dto1D(size_t x, size_t y, size_t z, const std::array<size_t, 3> &dim)
{
    return x + dim[0] * (y + dim[1] * z);
}

std::array<size_t, 3> map1Dto3D(size_t index, const std::array<size_t, 3> &dim)
{
    size_t z = index / (dim[0] * dim[1]);
    size_t y = (index - z * dim[0] * dim[1]) / dim[0];
    size_t x = index - z * dim[0] * dim[1] - y * dim[0];
    return {x, y, z};
}

int compact_color(int *color, int numnod)
{
    std::unordered_map<int, int> color_map;
    int new_color = 0;
    int old_color = UNCLASSIFIED;
    for (int i = 0; i < numnod; ++i)
    {
        if(color[i] == old_color)
        {
            continue;
        }
        old_color = color[i];
        if (color_map.find(old_color) == color_map.end())
        {
            color_map[old_color] = new_color++;
        }
    }

    for (int i = 0; i < numnod; ++i)
    {
        color[i] = color_map[color[i]];
    }
    return new_color;
}

extern "C" int cluster_nodes(const double *coords, int numnod, int *color, double eps, int minPts)
{
    // Initialize all nodes as unclassified
    // coords is of size 3*numnod [x1, y1, z1, x2, y2, z2, ...]
    std::fill(color, color + numnod, UNCLASSIFIED);

    int clusterID = 0;
    const double h = std::min(1e-2, eps);

    // create a bounding box for the clould of points
    std::array<double, 3> min_coords = {coords[0], coords[1], coords[2]};
    std::array<double, 3> max_coords = {coords[0], coords[1], coords[2]};
    for (size_t i = 0; i < numnod; i++)
    {
        min_coords[0] = std::min(min_coords[0], coords[3 * i]    - h);
        min_coords[1] = std::min(min_coords[1], coords[3 * i + 1] -h );
        min_coords[2] = std::min(min_coords[2], coords[3 * i + 2] -h );

        max_coords[0] = std::max(max_coords[0], coords[3 * i]    + h); 
        max_coords[1] = std::max(max_coords[1], coords[3 * i + 1]+ h);
        max_coords[2] = std::max(max_coords[2], coords[3 * i + 2]+ h);
    }
    // Calculate the size of the bounding box
    std::array<double, 3> sizes = {max_coords[0] - min_coords[0], max_coords[1] - min_coords[1], max_coords[2] - min_coords[2]};
    // case of degenerated boxes, set the size to 1
    std::array<size_t, 3> num_cells = {static_cast<size_t>(std::ceil(sizes[0] / eps)), static_cast<size_t>(std::ceil(sizes[1] / eps)), static_cast<size_t>(std::ceil(sizes[2] / eps))};

    // cap the number of cells between 1 and 128 in each dimension
    size_t soft_max_cells = std::pow(static_cast<double>(numnod) / static_cast<double>(minPts), 1.0 / 3.0);
    // soft_max_cells = std::min(soft_max_cells, static_cast<double>(MAX_CELLS));
    if(soft_max_cells < 1)
    {
        soft_max_cells = 1;
    }
    if(soft_max_cells > MAX_CELLS)
    {
        soft_max_cells = MAX_CELLS;
    }
    for (size_t i = 0; i < 3; ++i)
    {
        if( num_cells[i] > soft_max_cells)
        {
            num_cells[i] = static_cast<size_t>(soft_max_cells);
        }
        if (num_cells[i] < 1)
            num_cells[i] = 1;
    }
    // write the number of cells in each direction
    std::cout << numnod << " Number of cells: " << num_cells[0] << " " << num_cells[1] << " " << num_cells[2] << std::endl;

    // create the 3D integer coordinates of each vertex
    std::vector<std::array<size_t, 3>> cell_coords(numnod);
    const double lx = sizes[0] / static_cast<double>(num_cells[0]); 
    const double ly = sizes[1] / static_cast<double>(num_cells[1]);
    const double lz = sizes[2] / static_cast<double>(num_cells[2]);
    for (size_t i = 0; i < numnod; ++i)
    {
        cell_coords[i][0] = static_cast<size_t>(std::floor((coords[3 * i] - min_coords[0]) / lx));
        cell_coords[i][1] = static_cast<size_t>(std::floor((coords[3 * i + 1] - min_coords[1]) / ly));
        cell_coords[i][2] = static_cast<size_t>(std::floor((coords[3 * i + 2] - min_coords[2]) / lz));
        // clamp the coordinates to the bounding box
        cell_coords[i][0] = clamp(cell_coords[i][0], size_t(0), num_cells[0] - 1);
        cell_coords[i][1] = clamp(cell_coords[i][1], size_t(0), num_cells[1] - 1);
        cell_coords[i][2] = clamp(cell_coords[i][2], size_t(0), num_cells[2] - 1);

//        cell_coords[i][0] = clamp(cell_coords[i][0], static_cast<size_t>(0), soft_max_cells- 1);
//        cell_coords[i][1] = clamp(cell_coords[i][1], static_cast<size_t>(0), soft_max_cells- 1);
//        cell_coords[i][2] = clamp(cell_coords[i][2], static_cast<size_t>(0), soft_max_cells- 1);
    }

    // create a map of cells to nodes
    std::unordered_map<size_t, std::vector<size_t>> cell_map;
    for (size_t i = 0; i < numnod; ++i)
    {
        size_t cell_index = map3Dto1D(cell_coords[i][0], cell_coords[i][1], cell_coords[i][2], num_cells);
        if(cell_index >= num_cells[0] * num_cells[1] * num_cells[2])
        {
            std::cout << "Error: cell index out of bounds: " << cell_index << std::endl;
            std::cout<<"Cell coordinates: " << cell_coords[i][0] << " " << cell_coords[i][1] << " " << cell_coords[i][2] << std::endl;
            continue;
        }
        cell_map[cell_index].push_back(i);
        color[i] = cell_index;
    }

    clusterID = compact_color(color, numnod);
    std::cout<<"1 Number of clusters: " << clusterID << std::endl;
    // count the number of non-empty cells: the number of keys in the map

    size_t num_cells_filled = cell_map.size();
    std::vector<int> color_count(num_cells_filled, 0);
    for (size_t i = 0; i < numnod; ++i)
    {
        if (color[i] != UNCLASSIFIED)
        {
            color_count[color[i]]++;
        }
    }

    // finds the center of each cell, fill with 0
    std::vector<std::array<double, 3>> cell_centers(clusterID, std::array<double, 3>{0.0, 0.0, 0.0});

    // for each cell, find the center of the cell
    // loop over the cell_map keys and value
    for (const auto &cell : cell_map)
    {
        //the cell color, is the color of its first node
        const size_t current_color = color[cell.second[0]];
        //loop over the cell nodes
        for(size_t i = 0; i < cell.second.size(); ++i)
        {
            if(current_color != color[cell.second[i]])
            {
                std::cout<<"Error: cell " << cell.first << " has nodes with different colors" << std::endl;
            }
            const size_t node_index = cell.second[i];
            // nodes coordinates
            const std::array<double, 3> node_coords = {coords[3 * node_index], coords[3 * node_index + 1], coords[3 * node_index + 2]};
            // add the coordinates to the cell center
            cell_centers[current_color][0] += node_coords[0];
            cell_centers[current_color][1] += node_coords[1];
            cell_centers[current_color][2] += node_coords[2];
        }
        // divide by the number of nodes in the cell
        cell_centers[current_color][0] /= static_cast<double>(cell.second.size());
        cell_centers[current_color][1] /= static_cast<double>(cell.second.size());
        cell_centers[current_color][2] /= static_cast<double>(cell.second.size());
    }
   // reassign the colors to the nodes
    std::fill(color_count.begin(), color_count.end(), 0);
    for (size_t i = 0; i < numnod; ++i)
    {
        if (color[i] != UNCLASSIFIED)
        {
            color_count[color[i]]++;
        }
    }

    //finds the maximum number of nodes of a color
    size_t max_color = 0;
    for (size_t i = 0; i < clusterID; ++i)
    {
        if (color_count[i] > color_count[max_color])
        {
            max_color = i;
        }
    }

    const size_t size_treshold = std::min(minPts,  color_count[max_color]);


    // for cells with a small number of nodes, find the closest cell center and reassign the color
    for(const auto& cell : cell_map)
    {
        const size_t current_color = color[cell.second[0]];
        // if the cell has more than minPts nodes, skip it
        if(cell.second.size() >= minPts)
        {
            continue;
        }
        //loop over the cell nodes
        for(const auto& node: cell.second)
        {
            // nodes coordinates
            const std::array<double, 3> node_coords = {coords[3 * node], coords[3 * node + 1], coords[3 * node + 2]};
            // find the closest cell center
            size_t min_dist_index = current_color;
            double min_dist = std::numeric_limits<double>::max();
//            for(size_t j = 0; j < clusterID; ++j)
            for(const auto & c : cell_map)
            {
                // the cell color, is the color of its first node
                // if the size of the cell is less than minPts, skip it
                if(c.second.size() < size_treshold)
                {
                    continue;
                }
                //J is the new color
                const size_t j = color[c.second[0]];

                // skip the cells with less than minPts nodes
                double dist = std::sqrt(std::pow(node_coords[0] - cell_centers[j][0], 2) + std::pow(node_coords[1] - cell_centers[j][1], 2) + std::pow(node_coords[2] - cell_centers[j][2], 2));
                if(dist < min_dist)
                {
                    min_dist = dist;
                    min_dist_index = j;
                }
            }
            if(min_dist_index != current_color)
            {
            //    std::cout<<"Node " << node <<" is moved from " << current_color << " to " << min_dist_index << std::endl;
                if(min_dist_index >= clusterID)
                {
                    std::cout<<"Error: node " << node << " wrong color:" << min_dist_index << std::endl;
                }
                color[node] = min_dist_index;
            } else
            {
                std::cout<<"Error: node " << node << " "<< current_color<<" is not assigned to a new color" << std::endl;
            }

        }
    }

    clusterID = compact_color(color, numnod);


    // reassign the colors to the nodes
    std::fill(color_count.begin(), color_count.end(), 0);
    for (size_t i = 0; i < numnod; ++i)
    {
        if (color[i] != UNCLASSIFIED)
        {
            color_count[color[i]]++;
        }
    }

    std::cout<<"2 Number of clusters: " << clusterID << std::endl;
    // print the number of nodes per color
   // compute the bounding box of each cluster
    std::vector<std::array<double, 3>> cluster_min_coords(clusterID, {std::numeric_limits<double>::max(), std::numeric_limits<double>::max(), std::numeric_limits<double>::max()});
    std::vector<std::array<double, 3>> cluster_max_coords(clusterID, {std::numeric_limits<double>::lowest(), std::numeric_limits<double>::lowest(), std::numeric_limits<double>::lowest()});

    // global min and max coordinates
    std::array<double, 3> global_min_coords = {std::numeric_limits<double>::max(), std::numeric_limits<double>::max(), std::numeric_limits<double>::max()};
    std::array<double, 3> global_max_coords = {std::numeric_limits<double>::lowest(), std::numeric_limits<double>::lowest(), std::numeric_limits<double>::lowest()};    
    for(size_t n = 1; n < numnod ; ++n)
    {
        int current_color = color[n];
        // upadte the cluster min and max coordinates of that color
        cluster_min_coords[current_color][0] = std::min(cluster_min_coords[current_color][0], coords[3 * n]);
        cluster_min_coords[current_color][1] = std::min(cluster_min_coords[current_color][1], coords[3 * n + 1]);
        cluster_min_coords[current_color][2] = std::min(cluster_min_coords[current_color][2], coords[3 * n + 2]);
        cluster_max_coords[current_color][0] = std::max(cluster_max_coords[current_color][0], coords[3 * n]);
        cluster_max_coords[current_color][1] = std::max(cluster_max_coords[current_color][1], coords[3 * n + 1]);
        cluster_max_coords[current_color][2] = std::max(cluster_max_coords[current_color][2], coords[3 * n + 2]);
        // update the global min and max coordinates
        global_min_coords[0] = std::min(global_min_coords[0], coords[3 * n]);
        global_min_coords[1] = std::min(global_min_coords[1], coords[3 * n + 1]);
        global_min_coords[2] = std::min(global_min_coords[2], coords[3 * n + 2]);
        global_max_coords[0] = std::max(global_max_coords[0], coords[3 * n]);
        global_max_coords[1] = std::max(global_max_coords[1], coords[3 * n + 1]);
        global_max_coords[2] = std::max(global_max_coords[2], coords[3 * n + 2]);
    }

    double global_volume = (global_max_coords[0] - global_min_coords[0]) * (global_max_coords[1] - global_min_coords[1]) * (global_max_coords[2] - global_min_coords[2]);
    double sum_volume = 0.0;
    if(global_volume > 0.0)
    {
      for (size_t i = 0; i < clusterID; ++i)
      {
          double cluster_volume = (cluster_max_coords[i][0] - cluster_min_coords[i][0]) * (cluster_max_coords[i][1] - cluster_min_coords[i][1]) * (cluster_max_coords[i][2] - cluster_min_coords[i][2]);
          double cluster_ratio = cluster_volume / global_volume;
          sum_volume += cluster_volume;
          std::cout << "Color " << i << ": " << color_count[i] << " nodes" << " volume ratio: " << cluster_ratio*100.0<<"%"<< std::endl;
      }
    } 
 
    return clusterID;
}
