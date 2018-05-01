import os
import re
import json
import jinja2

generated_files = []

for dirpath, dirnames, filenames in os.walk('aws-sdk-ruby/apis'):
    if 'docs-2.json' not in filenames:
        continue

    with open(os.path.join(dirpath, 'api-2.json')) as f:
        api = json.load(f)

    if api['metadata']['protocol'] not in ['query', 'json', 'rest-json']:
        print('Skipped %s: %s' % (dirpath, api['metadata']['protocol']))
        continue

    print('Procwssing %s' % dirpath)

    operations = sorted(
            [(name, operation) for name, operation in api['operations'].items()])

    atomic_types = sorted(
            [(name, shape) for name, shape in api['shapes'].items()
                if shape['type'] in [
                    'boolean', 'integer', 'long', 'float', 'double', 'string', 'timestamp', 'blob']])
    complex_types = sorted(
            [(name, shape) for name, shape in api['shapes'].items()
                if shape['type'] in ['map', 'list', 'structure'] and
                ('exception' not in shape or not shape['exception'])])
    exceptions = sorted(
            [(name, shape) for name, shape in api['shapes'].items()
                if 'exception' in shape and shape['exception']])


    def first_lower(s):
        return s[0].lower() + s[1:]

    def first_lower2(s):
        if s[0].islower():
            return s + "'"
        else:
            return s[0].lower() + s[1:]

    def first_upper(s):
        return s[0].upper() + s[1:]

    def first_upper2(s):
        if s[0].islower():
            return s[0].upper() + s[1:] + "'"
        else:
            return s[0].upper() + s[1:]

    def mangle(s):
        if s.lower() in ['type', 'op', 'of', 'end', 'signature', 'include', 'and', 'option']:
            return s.lower() + "'"
        else:
            return s.strip("_")

    sml_types = {name: first_lower2(mangle(name)) for name in api['shapes'].keys()}
    sml_constructors = {name: first_upper2(mangle(name)) for name in api['shapes'].keys()}
    sml_operations = {name: first_lower(mangle(name)) for name in api['operations'].keys()}

    for (name, shape) in complex_types + exceptions:
        if shape['type'] != 'structure':
            continue
        shape['sml_fields'] = {k: first_lower(mangle(k)) for k in shape['members'].keys()}

    uid = api['metadata']['uid']

    env = jinja2.Environment(
            loader=jinja2.FileSystemLoader("."),
            block_start_string='(*%',
            block_end_string='%*)',
            trim_blocks=True,
            lstrip_blocks=True)

    structure_name = "".join([segment.title() for segment in re.split(r'[\.-]', uid)])

    # render and write to file
    sml_file_name = uid + '.sml'
    sml_template = env.get_template("template.sml.in")
    sml_stream = sml_template.stream(
            metadata = api['metadata'],
            structure_name = structure_name,
            sml_types = sml_types,
            sml_constructors = sml_constructors,
            sml_operations = sml_operations,
            operations = operations,
            atomic_types = atomic_types,
            complex_types = complex_types,
            exceptions = exceptions)
    sml_stream.dump('generated/' + sml_file_name)
    generated_files.append(sml_file_name)

    cm_template = env.get_template("single.cm.in")
    cm_stream = cm_template.stream(
            structure_name = structure_name,
            sml_file_name = sml_file_name)
    cm_stream.dump('generated/' + uid + '.cm')

    mlb_template = env.get_template("single.mlb.in")
    mlb_stream = mlb_template.stream(
            structure_name = structure_name,
            sml_file_name = sml_file_name)
    mlb_stream.dump('generated/' + uid + '.mlb')

template = env.get_template("aws.cm.in")
cm = template.render(generated_files = generated_files)
with open('generated/aws.cm', 'w') as f:
    f.write(cm)
