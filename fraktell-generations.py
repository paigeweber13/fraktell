import math
import subprocess

golden_ratio=1.618

def main():
    # experiment_around_065i()
    # experiment_between_04_and_151()
    # some_pretty_ones()
    high_resolution_ones()

def run_julia_hs(size, ca, cb, output_dir='images'):
    c_str = "{:.3f}+{:.3f}i".format(ca, cb)
    subprocess.run(['stack', 'run', '1.5', str(size), str(size), '50',
                    output_dir + '/julia' + c_str + '.png', 'RPU', '1',
                    '(' + str(ca) + ' :+ ' + str(cb) +')'])
    print('image', c_str, 'created with c =', c_str)

def experiment_around_065i():
    # a = 0.64
    a = 0.646
    # while a < 0.68:
    while a < 0.66:
        run_julia_hs(1000, 0, a)
        a += 0.001

def experiment_between_04_and_151():
    a = 0.4
    while a < 1.51:
        run_julia_hs(100, 0, a)
        a += 0.01

def some_pretty_ones():
    size = 1000
    run_julia_hs(size, 1-golden_ratio, 0)
    run_julia_hs(size, golden_ratio-2, golden_ratio-1)
    run_julia_hs(size, golden_ratio-2, golden_ratio-1)
    run_julia_hs(size, -0.70176, -0.3842)
    run_julia_hs(size, -0.835, -0.2321)
    run_julia_hs(size, -0.8, -0.156)
    run_julia_hs(size, -0.7269, -0.1889)

def high_resolution_ones():
    size = 10000
    run_julia_hs(size, 0, 0.7885 * math.exp(-math.pi/16),
                 output_dir='images/high-res')
    run_julia_hs(size, 0.8, -0.156, output_dir='images/high-res')
    run_julia_hs(size, 0.835, -0.232, output_dir='images/high-res')
    run_julia_hs(size, 0.285, 0, output_dir='images/high-res')

if __name__ == '__main__':
    main()
