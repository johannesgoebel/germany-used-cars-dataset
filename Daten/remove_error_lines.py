import csv

def remove_error_lines(input_csv, output_csv, error_lines):
    with open(input_csv, 'r', newline='', encoding='utf-8') as infile, \
            open(output_csv, 'w', newline='', encoding='utf-8') as outfile:
        reader = csv.reader(infile)
        writer = csv.writer(outfile, quoting=csv.QUOTE_ALL)

        try:
            # Write header
            header = next(reader)
            writer.writerow(header)

            # Iterate through rows, skipping error lines
            for i, row in enumerate(reader, start=1):
                if i not in error_lines:
                    writer.writerow(row)

        except StopIteration:
            print("CSV file is empty or has no data.")

if __name__ == "__main__":
    # Replace 'input.csv' and 'output.csv' with your file names
    input_csv = 'data_with_sentiment.csv'
    output_csv = 'data_with_sentiment_error_rows_removed.csv'

    # Replace the list of error line numbers
    error_lines = [986, 1084, 1085, 1216, 1345, 1624, 2294, 3392, 5344, 9785, 11001, 11610, 11765, 12164, 12712, 14177, 14595, 14810, 16106, 17105, 18073, 18990, 18991, 18999, 19002, 19003, 19395, 19442, 20801, 20802, 20821, 20822, 22186, 22855, 24060, 25006, 25782, 26301, 26398, 26588, 26704, 28860, 29590, 29785, 29862, 30553, 31527, 34167, 34405, 34655, 35929, 36192, 38136, 38365, 38529, 38536, 39342, 43241, 45745, 47399, 48165, 48541, 49047, 49613, 49625, 49643, 49700, 49871, 49901, 50061, 53785, 53861, 56428, 56681, 57643, 64664, 66323, 67029, 67177, 68862, 69682, 70812, 71377, 73473, 73950, 74095, 74105, 75528, 76585, 76663, 76668, 76756, 76822, 78371, 79585, 80922, 82312, 82569, 83169, 83685, 85749, 87317, 88930, 95550, 96019, 96581, 96836, 96839, 98502, 100302, 101402, 101997, 102305, 103514, 103610, 105317, 106021, 106230, 106412, 108906, 110930, 111695, 111952, 113605, 113607, 113608, 113613, 114213, 114322, 114464, 114647, 114767, 115193, 115583, 115682, 115904, 116160, 116378, 116818, 117203, 117483, 120024, 120043, 120067, 121380, 122418, 124398, 124416, 124463, 124553, 124658, 124735, 124749, 124753, 124761, 124781, 124796, 124806, 124981, 125078, 125902, 125903, 126219, 127035, 127224, 128781, 128898, 129490, 132498, 132864, 133419, 133903, 136807, 137410, 137652, 137656, 138201, 138395, 138441, 139411, 140493, 141281, 142129, 142313, 145531, 145980, 146367, 146991, 147008, 147056, 147094, 148090, 148194, 148424, 148591, 148761, 150538, 151238, 151283, 151432, 152645, 152917, 153674, 154043, 154053, 155098, 155102, 156118, 157306, 157513, 157945, 158216, 160458, 161125, 161602, 163208, 167103, 168738, 168923, 171215, 174761, 174910, 175358, 176402, 177327, 177398, 177409, 177874, 179210, 180526, 180539, 180608, 181554, 182145, 182246, 183330, 183355, 186695, 186861, 189535, 190595, 193439, 193539, 194199, 194202, 196212, 198036, 198078, 199112, 199506, 201956, 203222, 204065, 204835, 205297, 205325, 206014, 206685, 207921, 208153, 208374, 208508, 208617, 208877, 209187, 209189, 209250, 209411, 209803, 209963, 210385, 210414, 211042, 211257, 211669, 211840, 211847, 212275, 212488, 212747, 212931, 213043, 213183, 213198, 213492, 213706, 213725, 213765, 213936, 214041, 214305, 214310, 214360, 214437, 214488, 214531, 214636, 214637, 214906, 214908, 215261, 215296, 215472, 215507, 216067, 216102, 216103, 216158, 216167, 216485, 217288, 217522, 218709, 219259, 219353, 220664]

    remove_error_lines(input_csv, output_csv, error_lines)
