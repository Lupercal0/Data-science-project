import csv


csvfile = open('../data/labour_backup.csv', newline='',encoding="utf-8")
spamreader = csv.DictReader(csvfile)


flag = 0
table_list = []
total_count = {}
for row in spamreader:
    if flag == 0:
        total_count = row
        #print(total_count)
        flag = 2
        col_name = row.keys()
        
    
    else:
        #print(row)
        rate_count = {}
        row_name = ''
        for key in row.keys():
            #print(col)
            if key == 'X':
                rate_count['X'] = row[key]
            else:
                #print(col)
                rate_count[key] = float(row[key].replace(',',''))/float(total_count[key].replace(',',''))
        table_list.append(rate_count)
csvfile.close()


with open("../data/rate.csv", 'w', newline="") as csvfile:
        writer = csv.DictWriter(csvfile, fieldnames=col_name)
        writer.writeheader()
        for data in table_list:
            writer.writerow(data)
