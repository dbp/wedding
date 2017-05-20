<bind tag="class">nopic</bind>
<apply template="_base">

    <form action="rsvp_data_merge">
        <input type="hidden" name="s" value="crup"/>
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
                <confirmed>✔ <a href="/rsvp_unconfirm?i=${id}">undo</a></confirmed>
            </td>
            <td>
                <a href="/rsvp?k=${k}"><k/></a>
            </td>
            <td> <people>
                <include><strong><name/></strong></include>
                <not-include><name/></not-include>
                <locked><a href="/rsvp_person_unlock?i=${id}">✎</a></locked>
                <not-locked>
                    <a href="/rsvp_person_lock?i=${id}">🔒</a>
                    <a href="/rsvp_person_delete?i=${id}" onclick="return confirm('Are you sure you want to remove ${name}?')">✗</a>
                </not-locked>,
                </people>
                <a href="/rsvp_person_add?i=${id}">Add</a>
            </td>
        </tr>
        </rsvps>
    </table>

    </form>

</apply>
