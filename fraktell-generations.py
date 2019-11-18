import math
import subprocess

golden_ratio=1.618

def run_julia_hs(size, ca, cb):
    c_str = "{:.3f}+{:.3f}i".format(ca, cb)
    subprocess.run(['stack', 'run', '1.5', str(size), str(size), '50',
                    'images/output' + c_str + '.png', 'RPU', '1',
                    '(' + str(ca) + ' :+ ' + str(cb) +')'])
    print('image', c_str, 'created with c =', c_str)

a = 0.4
while a < 0.88:
    run_julia_hs(100, 0, a)
    a += 0.01

# a = 0.64
# while a < 0.68:
#     run_julia_hs(100, 0, a)
#     a += 0.001
