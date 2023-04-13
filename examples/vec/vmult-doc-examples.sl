import("vec");
() = evalfile("../examples-common");

_auto_declare = 1;

vmessage("\nThe output of this script should match the vmult() calls");
vmessage("given in the vectorization chapter of the SLIRP docs.\n");

print(vmult([1,2,3], [5,5,5]));
vmessage("");

Arr = Int_Type[2,3];
Arr[0,*] = 5;
Arr[1,*] = 100;
print(vmult(Arr, [3,4,5]));
vmessage("");

Arr3D = Double_Type[2,2,3];
Arr3D[0, *, *] = Arr;
Arr3D[1, *, *] = 2*Arr;
result = vmult ( Arr3D, [7, 8 , 9]);
print(result[0,*,*]);
print(result[1,*,*]);
