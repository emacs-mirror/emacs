'''
Sphinx extensions for the MPS documentation.
See <http://sphinx.pocoo.org/extensions.html>
'''

from docutils import nodes
from sphinx.util.compat import Directive, make_admonition

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

def visit_note_node(self, node):
    self.visit_admonition(node)

def depart_note_node(self, node):
    self.depart_admonition(node)

def setup(app):
    visit = (visit_note_node, depart_note_node)
    for d in NoteDirective.__subclasses__():
        app.add_node(d.cls, html = visit, latex = visit, text = visit)
        app.add_directive(d.cls.__name__, d)

class aka(Note):
    pass

class AkaDirective(NoteDirective):
    cls = aka
    label = 'Also known as'

class bibref(Note):
    pass

class BibrefDirective(NoteDirective):
    cls = bibref
    label = 'Related publication'

class historical(Note):
    pass

class HistoricalDirective(NoteDirective):
    cls = historical
    label = 'Historical note'

class link(Note):
    pass

class LinkDirective(NoteDirective):
    cls = link
    label = 'Related link'

class mps(Note):
    pass

class MpsDirective(NoteDirective):
    cls = mps
    label = 'In the MPS'

class opposite(Note):
    pass

class OppositeDirective(NoteDirective):
    cls = opposite
    label = 'Opposite'

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

class SimilarDirective(NoteDirective):
    cls = similar
    label = 'Similar'

class topics(Note):
    pass

class TopicsDirective(NoteDirective):
    cls = topics
    label = 'Topic'
