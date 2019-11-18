import subprocess
import time

array_types = ['VU', 'VS', 'RSU', 'RPU', 'RSS', 'RPS']

def print_csv_header():
    print('num Mpixels, array representation, num procs, time (s), Mpixels/s')

def run_julia_hs(size, num_procs, array_type):
    start_time = time.time()
    subprocess.run(['stack', 'run', 
                    '+RTS', '-N' + str(num_procs), 
                    '-RTS', '1.5', str(size), str(size), '100',
                    'images/output.png', array_type, '1', '(0.285 :+ 0)'])
    end_time = time.time()

    duration = end_time - start_time
    num_pixels = size ** 2

    pixels_to_mpixels = 1e-6
    print(num_pixels * pixels_to_mpixels, array_type, num_procs, duration, 
          num_pixels/duration * pixels_to_mpixels, sep=', ')

def main():
    print('building before running and timing tests...')
    subprocess.run(['stack', 'build'])
    print('done building!')

    print('\ntesting for various array representations:')
    print_csv_header()
    for array_type in array_types:
        run_julia_hs(2000, 12, array_type)

    print('\ntesting for various image sizes (sequential):')
    print_csv_header()
    for size in range(1000, 5001, 1000):
        run_julia_hs(size, 1, 'RPU')

    print('\ntesting for various image sizes (parallel):')
    print_csv_header()
    for size in range(1000, 5001, 1000):
        run_julia_hs(size, 12, 'RPU')

    print('\ntesting for various number or processors:')
    print_csv_header()
    for num_procs in range(1, 17):
        run_julia_hs(2000, num_procs, 'RPU')

if __name__ == '__main__':
    main()
