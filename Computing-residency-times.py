######
# tiburones1.py
# 11 de septiembre de 2017
######

ent=('2017-09-10_BUL resid by reef_freq.csv',
     '2017-09-10_GRS resid by reef_freq.csv',
     '2017-09-10_STS resid by reef_freq.csv')
sal=('bul.csv','grs.csv','sts.csv')

def pondt(r2,rf,frec,num):
      dt='%s,%s,%s,%s,%d,%d\n'%(tag,r2[2],r2[3],rf,int(frec),int(num))
      w.write(dt)
      
for k,entr in enumerate(ent):
   w=open(sal[k],'w')
   f=open(entr)
   hd=f.readline()[:-1].split(',')
   hd1=hd[0]+','+hd[2]+','+hd[3]+','+hd[6]+','+hd[7]+',acum\n'
   w.write(hd1); print(hd1)
   r1=f.readline()[:-1].split(',')
   # líneas para depuración
   #for i,s in enumerate(hd):
   #   print(i,s,r1[i],type(r1[i]))

   rf=r1[6]; num=1; tag=r1[0]; numtib=0; frec=int(r1[7])
   while(True):
     r2 = f.readline()[:-1].split(',')
     if len(r2[0])==0:
        r2=n; pondt(r2,rf,frec,num); break
     if r2[6]!=rf:
        pondt(r2,rf,frec,num)
        #print(tag,rf, num);
        rf=r2[6]; num=0; frec=0
     
     num+=1; frec+=int(r2[7]); n=r2 #n es el ultimo renglon
     if tag!=r2[0]:
        tag=r2[0]; numtib+=1

   f.close()
   w.close()
   
print('proceso terminado.')
