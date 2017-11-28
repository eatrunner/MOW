# scripts replaces every ';' with ',' so it will be possible to load
# file with read_csv()
import sys

redWineFile = 'winequality-red.csv'
whiteWineFile = 'winequality-white.csv'

f = open("winequality/" + redWineFile, 'r')
s1 = list(f.read())
f.close()

f = open("winequality/" + whiteWineFile, 'r')
s2 = list(f.read())
f.close()

for i in range(len(s1)):
	if (s1[i] == ';'):
		s1[i] = ','

for i in range(len(s2)):
	if (s2[i] == ';'):
		s2[i] = ','

f = open("winequality/" + redWineFile, 'w')
f.write("".join(s1))
f.close()

f = open("winequality/" + whiteWineFile, 'w')
f.write("".join(s2))
f.close()

