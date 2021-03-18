val=104
while (val>=2) {
 if (val%2==0) next=val/2
 else next=3*val+1
 print val, next
 val=next
}
