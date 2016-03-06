'''
This is Tutorial about how to extract online store's name and their hits
In the end, we can print out the visiting rank.
'''

import urllib2 # it's python2.7 so , urllib.request is not available.
from bs4 import BeautifulSoup

TARGET = "http://www.style-chart.com/rank/?&sort=F&page="
PAGES = 28

for index in range(1,PAGES): #assign 28 to index onebyon
    url = urllib2.urlopen(TARGET+str(index+1)) #open the each page
    data = url.read()
    soup = BeautifulSoup(data,"html.parser") #ready for parsing, so read the each page
    query = soup.find_all('li', attrs={'class':'info2'})  # find tag 'li which has the classname 'info2'.
    url.close() #close the each page
    for child in query: #in query, there are class attributes,
        try:
            print("%s\t%s" %
                  (child.contents[1].strong.string, child.contents[-4].contents[-1].string) #.contents method is used when you call tag's children. check http://www.crummy.com/software/BeautifulSoup/bs4/doc/
                  )
        except AttributeError: #if there is no content, pass
            pass