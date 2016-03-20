# coding=utf-8
import sys
import random
reload(sys)
sys.setdefaultencoding('cp949') # 기본코드를 utf-8 로 하여 읽는데 한글로 읽는것에 문제가 없게 함.

file = open('menu.txt')
lines= file.readlines()
count=len(lines)

#Menu = ['중식','일식','한식']

#for x in Menu:
#    pick = x.encode('cp949') #이 for 문은 그냥 한글로 나오는 메뉴 리스트를 만들고 싶었음.

index= random.randrange(0,count)

print lines[index].encode('cp949') # 여기가 좀 이해가 안감. 읽는것을 이미 setdefaultencoding 을 사용해 utf로 하여 한글로 인코딩이 되었을텐데 저 encode
                                    # 메소드를 뺀다면 글씨가 깨짐.
                                    # 고민해결! 윈도우에서 기본 인코딩은 cp949 (ANSI 에서 MS 는 cp949 이다.)
                                    # utf-8 과 cp949는 같은 한글 인코딩이더라도 구조가 다르다. 따라서 처음 내가 utf로 인코딩을 해줬더라도 윈도우 환경에서의 기본 인코딩인 cp949와 다른 구조이므로 깨짐.
                                    # cp949는 자모음 조합에 번호를 붙인 인코딩이고, utf-8은 자음 모음 각자에 전부다 번호를 준 인코딩
                                    # 그래서 호환이 안됨.

