<bind tag="class">nopic</bind>
<apply template="_base">

    <form action="/data/merge">
        <input type="hidden" name="s" value="${s}"/>
        <table cellspacing="0">
        <tr><th><input style="position: fixed; left: 100px; font-weight: bold; font-size: 20px;" type="submit" value="MERGE"/></th>
            <th>Confirmed</th>
            <th>URL</th>
            <th>People</th>
        </tr>
        <rsvps>
            <tr>
                <td>
                    <input type="checkbox" name="merge" value="${id}"/>
                </td>
            <td>
                <confirmed>✔ <a href="/data/unconfirm?i=${id}&s=${s}">undo</a></confirmed>
            </td>
            <td>
                <a href="/rsvp?k=${k}"><k/></a>
            </td>
            <td> <people>
                <include><strong><name/></strong></include>
                <not-include><name/></not-include>
                <locked><a href="/data/person_unlock?i=${id}&s=${s}">✎</a></locked>
                <not-locked>
                    <a href="/data/person_lock?i=${id}&s=${s}">🔒</a>
                    <a href="/data/person_delete?i=${id}&s=${s}" onclick="return confirm('Are you sure you want to remove ${name}?')">✗</a>
                </not-locked>,
                </people>
                <a href="/data/person_add?i=${id}&s=${s}">Add</a>
            </td>
        </tr>
        </rsvps>
    </table>

    </form>

</apply>
