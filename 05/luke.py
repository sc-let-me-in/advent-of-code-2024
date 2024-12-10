with open("../inputs/5", "r") as input_file:
    s = input_file.read().split("\n\n")
    
ipt = {"rules": s[0].split("\n"), "lists": s[1].split("\n")}

ipt["rules"] = [[int(sti) for sti in st.split("|")] for st in ipt["rules"]]
rules = {}
for rule in ipt["rules"]:
    if rule[1] not in rules:
        rules[rule[1]] = [rule[0]]
    else:
        rules[rule[1]].append(rule[0])


ipt["lists"].pop(len(ipt["lists"]) - 1)
lists = [[int(s) for s in ls.split(",")] for ls in ipt["lists"]]


def is_legal_list(l, rules):
    banlist = []
    for i in l:
        if i in banlist:
            return False
        elif i in rules:
            for ban in rules[i]:
                if ban not in banlist:
                    banlist.append(ban)
    
    return True


legal_lists = [l for l in lists if is_legal_list(l, rules)]
legal_sum = [int(l[int(len(l) / 2)]) for l in legal_lists]
print(rules)
print(legal_sum)
print(sum(legal_sum))
