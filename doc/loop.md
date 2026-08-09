# Explicit time loop summary

1. **Control / sensors / activation**  

2. **Early external loads**   $F_{\mathrm{ext}} \leftarrow F_{\mathrm{ext}} + F_{\mathrm{force}} + F_{\mathrm{pinch}} + F_{\mathrm{finger}} + F_{\mathrm{fluid}} + F_{\mathrm{press}}$

3. **Contact sort / candidates**  

4. **Contact forces**  $F_{\mathrm{cont}} \leftarrow F_{\mathrm{cont}} + f_{\mathrm{contact}}$  

5. **FE Internal forces**  $F_{\mathrm{int}} \leftarrow F_{\mathrm{int}} + \sum_e f_{\mathrm{int},e}$  

6. **Nodal assembly + global MPI exchange**  $F_{\mathrm{node}} \leftarrow \sum F_{\mathrm{local}} + \sum F_{\mathrm{remote}}$  

7. **Rigid bodies / walls / extra force terms**  $F \leftarrow F + F_{\mathrm{rby}} + F_{\mathrm{wall}} + \cdots$  

8. **Time step evaluation**  $\Delta t = \min(\Delta t_{\mathrm{nodal}}, \Delta t_{\mathrm{damp}}, \ldots)$  

9. **Acceleration update**     $A = F/M$  

10. **Kinematic constraints on nodal state**  

11. **Explicit integration**  update V, D and X                                                                                                                                         
12. **Time / cycle advance and stop logic**   $TT \leftarrow TT + \Delta t$ ; $NCYCLE \leftarrow NCYCLE + 1$  

