
from pyswip import Prolog, newModule
from multiprocessing import Process
from time import sleep

def test_multiprocess():
    def test_process(name, args):
        module = newModule(name)
        prolog = Prolog(module)
        list(prolog.query("dynamic(father/2)"))
        for arg in args:
            result = list(prolog.query(f"assertz(father(michael,{arg}))"))
        sleep(2)
        for sol in prolog.query("father(michael,X)"):
            print(f"{name}: {sol['X'] in args}")

        print(list(prolog.query("father(michael, jack)")))
        sleep(10)


    p = Process(target=test_process, args=('process1', ['jack', 'lewis', 'james', 'logan', 'daniel', 'ryan', 'aaron', 'oliver', 'liam', 'jamie', 'ethan'],))
    p2 = Process(target=test_process, args=('process2', ['alexander', 'cameron', 'finlay', 'kyle', 'adam', 'harry', 'matthew', 'callum', 'lucas', 'nathan', 'aiden', 'dylan', 'charlie'],))

    p.start()
    sleep(2)
    p2.start()
    p.join()
    p2.join()


if __name__ == "__main__":
    test_multiprocess()