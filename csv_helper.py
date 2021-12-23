import csv


def write_csv(data, filename, column_names):
  """
  """
  with open('%s.csv' % filename, 'w') as file:
    writer = csv.DictWriter(file, fieldnames=column_names)                                               
    writer.writeheader()
    for row in data:
      writer.writerow(row)

    print('[write_csv] wrote %s rows to \'%s\'' % (len(data), filename))


def read_csv(filename, column_names):
    """
    """
    csv_file = open(filename)
    reader = csv.DictReader(csv_file)
    data = []
    for row in reader:
        datum = {}
        for column in column_names:
            datum[column] = row[column]
        data.append(datum)

    csv_file.close()

    print('[read_csv] read %s rows from \'%s\'' % (len(data), filename))

    return data