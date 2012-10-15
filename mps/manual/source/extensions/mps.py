'''
Sphinx extensions for the MPS documentation.
See <http://sphinx.pocoo.org/extensions.html>
'''

from collections import defaultdict
import re
from docutils import nodes, transforms
from sphinx import addnodes
from sphinx.domains import Domain
from sphinx.roles import XRefRole
from sphinx.util.compat import Directive, make_admonition

class MpsDomain(Domain):
    label = 'MPS'
    name = 'mps'

class Note(nodes.Admonition, nodes.Element):
    pass

class NoteDirective(Directive):
    cls = None
    label = 'Note'
    has_content = True

    def run(self):
        ad = make_admonition(self.cls, self.name, [self.label], self.options,
                             self.content, self.lineno, self.content_offset,
                             self.block_text, self.state, self.state_machine)
        return ad

class PluralDirective(NoteDirective):
    def run(self):
        ad = make_admonition(self.cls, self.name, [self.label], self.options,
                             self.content, self.lineno, self.content_offset,
                             self.block_text, self.state, self.state_machine)
        refs = sum(1 for node in ad[0].children[1].children
                   if isinstance(node, addnodes.pending_xref)
                   or isinstance(node, nodes.Referential))
        if refs > 1:
            assert(isinstance(ad[0].children[0], nodes.title))
            ad[0].children[0].children[0] = nodes.Text(self.plural)
        return ad

def visit_note_node(self, node):
    self.visit_admonition(node)

def depart_note_node(self, node):
    self.depart_admonition(node)

class aka(Note):
    pass

class AkaDirective(NoteDirective):
    cls = aka
    label = 'Also known as'

class bibref(Note):
    pass

class BibrefDirective(PluralDirective):
    cls = bibref
    label = 'Related publication'
    plural = 'Related publications'

class historical(Note):
    pass

class HistoricalDirective(NoteDirective):
    cls = historical
    label = 'Historical note'

class link(Note):
    pass

class LinkDirective(PluralDirective):
    cls = link
    label = 'Related link'
    plural = 'Related links'

class opposite(Note):
    pass

class OppositeDirective(PluralDirective):
    cls = opposite
    label = 'Opposite term'
    plural = 'Opposite terms'

class relevance(Note):
    pass

class RelevanceDirective(NoteDirective):
    cls = relevance
    label = 'Relevance to memory management'

class see(Note):
    pass

class SeeDirective(NoteDirective):
    cls = see
    label = 'See'

class similar(Note):
    pass

class SimilarDirective(PluralDirective):
    cls = similar
    label = 'Similar term'
    plural = 'Similar terms'

class specific(Note):
    pass

class SpecificDirective(NoteDirective):
    domain = 'mps'
    cls = specific
    label = 'In the MPS'

class topics(Note):
    pass

class TopicsDirective(PluralDirective):
    cls = topics
    label = 'Topic'
    plural = 'Topics'

all_directives = [
    AkaDirective,
    BibrefDirective,
    HistoricalDirective, 
    LinkDirective,
    OppositeDirective,
    RelevanceDirective,
    SeeDirective,
    SimilarDirective,
    SpecificDirective, 
    TopicsDirective]

see_only_ids = set()
xref_ids = defaultdict(list)

class GlossaryTransform(transforms.Transform):
    default_priority = 999
    sense_re = re.compile(r'(.*)\s+(\([0-9]+\))$')

    def apply(self):
        global see_only_ids, xref_ids
        for target in self.document.traverse(nodes.term):
            target.children = list(self.edit_children(target))
        for target in self.document.traverse(addnodes.pending_xref):
            if target['reftype'] == 'term':
                xref_ids['term-{}'.format(target['reftarget'])].append((target.source, target.line))
                c = target.children
                if len(c) == 1 and isinstance(c[0], nodes.emphasis):
                    c[0].children = list(self.edit_children(c[0]))
        for target in self.document.traverse(nodes.definition_list_item):
            ids = set()
            for c in target.children:
                if isinstance(c, nodes.term):
                    ids = set(c['ids'])
                if (isinstance(c, nodes.definition)
                    and len(c.children) == 1
                    and isinstance(c.children[0], see)):
                    see_only_ids |= ids

    def edit_children(self, target):
        for e in target.children:
            if not isinstance(e, nodes.Text):
                yield e
                continue
            m = self.sense_re.match(e)
            if not m:
                yield e
                continue
            yield nodes.Text(m.group(1))
            yield nodes.superscript(text = m.group(2))

def warn_indirect_terms(app, exception):
    if not exception:
        for i in see_only_ids:
            for doc, line in xref_ids[i]:
                print('Cross-reference to {} at {} line {}.'.format(i, doc, line))

def setup(app):
    app.add_domain(MpsDomain)
    app.add_transform(GlossaryTransform)
    app.connect('build-finished', warn_indirect_terms)

    visit = (visit_note_node, depart_note_node)
    for d in all_directives:
        app.add_node(d.cls, html = visit, latex = visit, text = visit)
        try:
            app.add_directive_to_domain(d.domain, d.cls.__name__, d)
        except AttributeError:
            app.add_directive(d.cls.__name__, d)
