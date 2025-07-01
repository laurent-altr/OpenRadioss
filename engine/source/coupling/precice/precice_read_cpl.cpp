#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <cstring>
#include <iostream>

extern "C"
{
    void cpp_precice_read_cpl(char *input_filename, char *participant_name, char *config_file, char *mesh_name, char *write_name, char *read_name, int *grnod_id)
    {
        std::ifstream file(input_filename);
        std::string line;
        std::vector<std::vector<std::string>> lines;
        char tmp[50];

        while (std::getline(file, line))
        {
            if (line.rfind("/PRECICE", 0) == 0)
            { // If line starts with "/preCICE"
                std::vector<std::string> words;
                std::string word;
                std::istringstream ss(line);
                while (std::getline(ss, word, '/'))
                {
                    words.push_back(word);
                }
                lines.push_back(words);
            }
        }

        // Find the desired data and copy it to the char pointers
        for (const auto &words : lines)
        {
            if (words[2] == "PARTICIPANT_NAME")
            {
                std::strcpy(participant_name, words[3].c_str());
            }
            else if (words[2] == "CONFIG_FILE")
            {
                std::strcpy(config_file, words[3].c_str());
            }
            else if (words[2] == "MESH_NAME")
            {
                std::strcpy(mesh_name, words[3].c_str());
            }
            else if (words[2] == "WRITE")
            {
                std::strcpy(write_name, words[3].c_str());
            }
            else if (words[2] == "READ")
            {
                std::strcpy(read_name, words[3].c_str());
            }
            else if (words[2] == "INTERFACE")
            {
                // convert words[3] into integer
                *grnod_id = atoi(words[3].c_str());
            }
        }
    }
}
