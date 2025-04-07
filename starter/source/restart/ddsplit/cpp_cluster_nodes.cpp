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
    for (int i = 0; i < numnod; ++i)
    {
        if (color[i] != UNCLASSIFIED)
        {
            if (color_map.find(color[i]) == color_map.end())
            {
                color_map[color[i]] = new_color++;
            }
            color[i] = color_map[color[i]];
        }
    }
    return new_color;
}

extern "C" int cluster_nodes(const double *coords, int numnod, int *color, double eps, int minPts)
{
    // Initialize all nodes as unclassified
    // coords is of size 3*numnod [x1, y1, z1, x2, y2, z2, ...]
    std::fill(color, color + numnod, UNCLASSIFIED);

    int clusterID = 0;

    // create a bounding box for the clould of points
    std::array<double, 3> min_coords = {coords[0], coords[1], coords[2]};
    std::array<double, 3> max_coords = {coords[0], coords[1], coords[2]};
    for (size_t i = 0; i < numnod; i++)
    {
        min_coords[0] = std::min(min_coords[0], coords[3 * i]);
        min_coords[1] = std::min(min_coords[1], coords[3 * i + 1]);
        min_coords[2] = std::min(min_coords[2], coords[3 * i + 2]);

        max_coords[0] = std::max(max_coords[0], coords[3 * i]);
        max_coords[1] = std::max(max_coords[1], coords[3 * i + 1]);
        max_coords[2] = std::max(max_coords[2], coords[3 * i + 2]);
    }
    // Calculate the size of the bounding box
    std::array<double, 3> sizes = {max_coords[0] - min_coords[0], max_coords[1] - min_coords[1], max_coords[2] - min_coords[2]};
    std::array<size_t, 3> num_cells = {static_cast<size_t>(std::ceil(sizes[0] / eps)), static_cast<size_t>(std::ceil(sizes[1] / eps)), static_cast<size_t>(std::ceil(sizes[2] / eps))};

    // cap the number of cells between 1 and 128 in each dimension
    for (size_t i = 0; i < 3; ++i)
    {
        if (num_cells[i] < 1)
            num_cells[i] = 1;
        if (num_cells[i] > MAX_CELLS)
            num_cells[i] = MAX_CELLS;
        if( num_cells[i] > std::pow(numnod, 1.0/3.0))
        {
            num_cells[i] = static_cast<size_t>(std::pow(numnod, 1.0/3.0));
        }
    }


    // write the number of cells in each direction
    std::cout << numnod << " Number of cells: " << num_cells[0] << " " << num_cells[1] << " " << num_cells[2] << std::endl;

    // create the 3D integer coordinates of each vertex
    std::vector<std::array<size_t, 3>> cell_coords(numnod);

    for (size_t i = 0; i < numnod; ++i)
    {
        cell_coords[i][0] = static_cast<size_t>(std::floor((coords[3 * i] - min_coords[0]) / eps));
        cell_coords[i][1] = static_cast<size_t>(std::floor((coords[3 * i + 1] - min_coords[1]) / eps));
        cell_coords[i][2] = static_cast<size_t>(std::floor((coords[3 * i + 2] - min_coords[2]) / eps));
        // clamp the coordinates to the bounding box
        cell_coords[i][0] = clamp(cell_coords[i][0], static_cast<size_t>(0), MAX_CELLS - 1);
        cell_coords[i][1] = clamp(cell_coords[i][1], static_cast<size_t>(0), MAX_CELLS - 1);
        cell_coords[i][2] = clamp(cell_coords[i][2], static_cast<size_t>(0), MAX_CELLS - 1);
    }

    // create a map of cells to nodes
    std::unordered_map<size_t, std::vector<size_t>> cell_map;
    for (size_t i = 0; i < numnod; ++i)
    {
        size_t cell_index = map3Dto1D(cell_coords[i][0], cell_coords[i][1], cell_coords[i][2], num_cells);
        cell_map[cell_index].push_back(i);
    }

    // count the number of non-empty cells: the number of keys in the map
    clusterID = 0;
    size_t num_cells_filled = cell_map.size();
    for (const auto &cell : cell_map)
    {
        // if cell already has a color, skip it
        // look for the color of the first node in the cell
        if (color[cell.second[0]] != UNCLASSIFIED)
        {
            continue;
        }

        // set of non-empty cells, in the neighborhood of the current cell
        std::set<size_t> neighbor_cells;
        neighbor_cells.insert(cell.first);
        std::set<size_t> new_neighbor_cells;

        std::queue<size_t> cell_queue;
        cell_queue.push(cell.first);

        while (!cell_queue.empty())
        {
            size_t current_cell = cell_queue.front();
            cell_queue.pop();

            // Get the coordinates of the current cell
            std::array<size_t, 3> cell_coord = map1Dto3D(current_cell, num_cells);

            // Assign the current color to all vertices of that cell
            for (const auto &node_index : cell_map[current_cell])
            {
                color[node_index] = clusterID;
            }

            // Check all 26 neighbors (3x3x3 cube minus the center)
            for (int dx = -1; dx <= 1; dx++)
            {
                for (int dy = -1; dy <= 1; dy++)
                {
                    for (int dz = -1; dz <= 1; dz++)
                    {
                        if (dx == 0 && dy == 0 && dz == 0)
                            continue; // Skip the center

                        size_t nx = clamp(cell_coord[0] + dx, static_cast<size_t>(0), num_cells[0] - 1);
                        size_t ny = clamp(cell_coord[1] + dy, static_cast<size_t>(0), num_cells[1] - 1);
                        size_t nz = clamp(cell_coord[2] + dz, static_cast<size_t>(0), num_cells[2] - 1);
                        size_t neighbor_index = map3Dto1D(nx, ny, nz, num_cells);

                        auto cell_it = cell_map.find(neighbor_index);
                        if (cell_it != cell_map.end() && color[cell_it->second[0]] == UNCLASSIFIED)
                        {
                            // Found an unclassified neighbor cell
                            cell_queue.push(neighbor_index);

                            // Mark all nodes in this cell immediately to prevent re-queueing
                            for (const auto &node_index : cell_it->second)
                            {
                                color[node_index] = clusterID;
                            }
                        }
                    }
                }
            }
        }
        clusterID++;
    }

    // check if there are unassigned nodes
    for (size_t i = 0; i < numnod; ++i)
    {
        if (color[i] == UNCLASSIFIED)
        {
            std::cout << "Unassigned node: " << i << std::endl;
        }
    }

    // writes the number of nodes per color
    std::vector<int> color_count(clusterID, 0);
    for (size_t i = 0; i < numnod; ++i)
    {
        color_count[color[i]]++;
    }
    // compute the AABB of each cluster, i.e. for each color
    std::vector<std::array<double, 3>> cluster_min(clusterID, {std::numeric_limits<double>::max(), std::numeric_limits<double>::max(), std::numeric_limits<double>::max()});
    std::vector<std::array<double, 3>> cluster_max(clusterID, {std::numeric_limits<double>::lowest(), std::numeric_limits<double>::lowest(), std::numeric_limits<double>::lowest()});
    for (size_t i = 0; i < numnod; ++i)
    {
        if (color[i] != UNCLASSIFIED)
        {
            cluster_min[color[i]][0] = std::min(cluster_min[color[i]][0], coords[3 * i]);
            cluster_min[color[i]][1] = std::min(cluster_min[color[i]][1], coords[3 * i + 1]);
            cluster_min[color[i]][2] = std::min(cluster_min[color[i]][2], coords[3 * i + 2]);

            cluster_max[color[i]][0] = std::max(cluster_max[color[i]][0], coords[3 * i]);
            cluster_max[color[i]][1] = std::max(cluster_max[color[i]][1], coords[3 * i + 1]);
            cluster_max[color[i]][2] = std::max(cluster_max[color[i]][2], coords[3 * i + 2]);
        }
    }



    // reassign each color of each vertex
    // For each node, finds all the bounding boxes it crosses. Pick the one with the most points
    std::vector<int> new_color(numnod, UNCLASSIFIED);
    for (size_t i = 0; i < numnod; ++i)
    {
        size_t current_color = color[i];
        // current color size
        size_t max_cluster_size = color_count[current_color];
        // loop over all bounding boxes
        if (color_count[current_color] < minPts)
        {
            for (size_t c = 0; c < clusterID; ++c)
            {
                if (color_count[c] < minPts)
                    continue;
                // check if the node is inside the bounding box
                if (coords[3 * i] >= cluster_min[c][0] && coords[3 * i] <= cluster_max[c][0] &&
                    coords[3 * i + 1] >= cluster_min[c][1] && coords[3 * i + 1] <= cluster_max[c][1] &&
                    coords[3 * i + 2] >= cluster_min[c][2] && coords[3 * i + 2] <= cluster_max[c][2])
                {
                    // check if the cluster size is bigger
                    if (color_count[c] > max_cluster_size)
                    {
                        current_color = c;
                        max_cluster_size = color_count[c];
                    }
                }
            }

            if(current_color == color[i])
            {
                double dist_min = std::numeric_limits<double>::max();
                for(size_t c = 0; c < clusterID; ++c)
                {
                    if(color_count[c] < minPts)
                        continue;
                    double dist = std::sqrt(std::pow(cluster_min[c][0] - coords[3 * i], 2) +
                                             std::pow(cluster_min[c][1] - coords[3 * i + 1], 2) +
                                             std::pow(cluster_min[c][2] - coords[3 * i + 2], 2));
                    if(dist < dist_min)
                    {
                        current_color = c;
                        dist_min = dist;
                    }
                }
            }
            if (color[i] != current_color)
            {
                new_color[i] = current_color;
                color_count[current_color]++;
                color_count[color[i]]--;
            }
        }
    }

    // copy new_color to color
    for (size_t i = 0; i < numnod; ++i)
    {
        if(new_color[i] != UNCLASSIFIED)
        {
            color[i] = new_color[i];
        }
        
    }

    int new_color_id = compact_color(color, numnod);

    // update clusterID to the number of clusters
    clusterID = new_color_id;

    // recount the number of nodes per color
    color_count.resize(clusterID, 0);
    std::fill(color_count.begin(), color_count.end(), 0);

    for (size_t i = 0; i < numnod; ++i)
    {
        if (color[i] != UNCLASSIFIED)
        {
            color_count[color[i]]++;
        }
    }

    // print the number of nodes per color
    for (size_t i = 0; i < clusterID; ++i)
    {
        std::cout << "Color " << i << ": " << color_count[i] << " nodes" << std::endl;
    }

    return clusterID;
}
